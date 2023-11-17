import { EditorState, Transaction } from "prosemirror-state";
import { toggleMark } from "prosemirror-commands";
import { Schema } from "prosemirror-model";

export const toggleBold = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const markType = state.schema.marks.strong;
  toggleMark(markType)(state, dispatch);
};

export const toggleItalic = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const markType = state.schema.marks.em;
  toggleMark(markType)(state, dispatch);
};

export const toggleUnderline = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const markType = state.schema.marks.underline;
  toggleMark(markType)(state, dispatch);
};

export const clearFormatting = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const { tr, schema, selection } = state;
  const { from, to } = selection;

  const marksToRemove = ["strong", "em", "underline", "textColor", "highlightColor"];

  marksToRemove.forEach((markName) => {
    const markType = schema.marks[markName];
    if (markType) {
      tr.removeMark(from, to, markType);
    }
  });

  state.doc.nodesBetween(from, to, (node, pos) => {
    if (node.type === schema.nodes.section) {
      return false;
    }
    if (node.isTextblock && node.type !== schema.nodes.paragraph) {
      tr.setNodeMarkup(pos, schema.nodes.paragraph);
    }
    return true;
  });

  if (tr.docChanged && dispatch) {
    dispatch(tr);
  }
};

export const setTextColor = (
  state: EditorState,
  dispatch: (tr: Transaction) => void,
  color: string
) => {
  const { from, to } = state.selection;
  const markType = state.schema.marks.textColor;
  let tr = state.tr.removeMark(from, to, markType);
  tr = tr.addMark(from, to, markType.create({ color }));
  dispatch(tr);
};

export const setHighlightColor = (
  state: EditorState,
  dispatch: (tr: Transaction) => void,
  color: string
) => {
  const { from, to } = state.selection;
  const markType = state.schema.marks.highlightColor;
  let tr = state.tr.removeMark(from, to, markType);
  tr = tr.addMark(from, to, markType.create({ color }));
  dispatch(tr);
};

export const removeHighlightColor = (state: EditorState, dispatch: (tr: Transaction) => void) => {
  const { from, to } = state.selection;
  const markType = state.schema.marks.highlightColor;
  const tr = state.tr.removeMark(from, to, markType);
  dispatch(tr);
};

export const getCurrentTextAndHighlightColors = (
  state: EditorState,
  setHighlightFillColor: (color: string | null) => void,
  setTextFillColor: (color: string | null) => void
) => {
  const { from } = state.selection;
  const highlightMarkType = state.schema.marks.highlightColor;
  const textMarkType = state.schema.marks.textColor;
  const marks = state.doc.resolve(from).marks();

  let highlightColorFound = false;
  let textColorFound = false;

  for (let mark of marks) {
    if (mark.type === highlightMarkType) {
      setHighlightFillColor(mark.attrs.color);
      highlightColorFound = true;
    }
    if (mark.type === textMarkType) {
      setTextFillColor(mark.attrs.color);
      textColorFound = true;
    }
    if (highlightColorFound && textColorFound) return;
  }

  if (!highlightColorFound) setHighlightFillColor(null);
  if (!textColorFound) setTextFillColor(null);
};
