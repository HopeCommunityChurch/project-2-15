import { EditorState, Transaction } from "prosemirror-state";
import { toggleMark } from "prosemirror-commands";
import { Schema } from "prosemirror-model";
import { textSchema } from "./textSchema";
import { Slice, Node, Mark, Fragment } from "prosemirror-model";
import { v4 as uuidv4 } from "uuid";

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

export const setTextColor = (color : string) => (
  state: EditorState,
  dispatch: (tr: Transaction) => void
) => {
  const { from, to } = state.selection;
  const markType = state.schema.marks.textColor;
  let tr = state.tr.removeMark(from, to, markType);
  tr = tr.addMark(from, to, markType.create({ color }));
  dispatch(tr);
};

export const setHighlightColor = (color: string) => (
  state: EditorState,
  dispatch: (tr: Transaction) => void
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


export type AddVerse = {
  book: string;
  chapter: number;
  verse: number;
  passage: string;
};

function mkVerseNode(verse: AddVerse): Node[] {
  const vMark = textSchema.marks.verse.create({
    book: verse.book,
    chapter: verse.chapter,
    verse: verse.verse,
  });
  return [textSchema.text(verse.passage, [vMark]), textSchema.text(" ")];
}

export let addVerse = (
  verseRef: string,
  passage: Array<AddVerse>,
) => (
  state: EditorState,
  dispatch?: (tr: Transaction) => void
) => {
  if (dispatch) {
    const selection = state.selection;

    let sectionNode = null;
    let sectionPos = null;
    state.doc.nodesBetween(selection.from, selection.to, (node, pos) => {
      if (node.type.name === "section") {
        sectionNode = node;
        sectionPos = pos;
      }
    });

    if (sectionNode && sectionPos !== null) {
      let posOfStudyBlock = null;
      let posOfSectionHeader = null;
      let sectionHeaderNode = null;
      let sectionHeaderSize = 0;
      sectionNode.forEach((childNode, offset) => {
        if (childNode.type.name === "studyBlocks") {
          posOfStudyBlock = sectionPos + offset;
        }
        if (childNode.type.name === "sectionHeader") {
          posOfSectionHeader = sectionPos + offset;
          sectionHeaderNode = childNode;
          sectionHeaderSize = childNode.nodeSize;
        }
      });

      let tr = state.tr;
      let headerDiff = 0;

      // Check if the section header is "Untitled"
      if (
        sectionHeaderNode &&
        sectionHeaderNode.textContent === "Untitled" &&
        posOfSectionHeader !== null
      ) {
        const newHeader = textSchema.text(verseRef);
        headerDiff = newHeader.nodeSize - sectionHeaderSize;
        tr = tr.replaceWith(posOfSectionHeader, posOfSectionHeader + sectionHeaderSize, newHeader);
      }

      // Adjust the position of studyBlocks based on the new header size
      if (posOfStudyBlock !== null) {
        posOfStudyBlock += headerDiff + 1;

        const textNode = passage.flatMap(mkVerseNode);
        const chunk = textSchema.nodes.chunk.create(null, textNode);
        const bibleText = textSchema.nodes.bibleText.create({ verses: verseRef }, chunk);
        tr = tr.insert(posOfStudyBlock, bibleText);
      }

      // Dispatch the transaction with the adjustments
      dispatch(tr);
      return true;
    }
  }
  return false;
};

export const addGeneralStudyBlock = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  if (dispatch) {
    // Get the section node where the cursor is currently positioned
    const sectionNode = state.selection.$anchor.node(1);
    let position = null;

    // Find the position right after the last child of the current section
    position = state.selection.$anchor.before(1) + sectionNode.content.size;

    // Create the nodes for the new study block
    const header = textSchema.text("Untitled");
    const sbHeader = textSchema.nodes.generalStudyBlockHeader.createChecked(null, header);
    const bodytxt = textSchema.text("my stuff here");
    const bodyp = textSchema.nodes.paragraph.createChecked(null, bodytxt);
    const sbBody = textSchema.nodes.generalStudyBlockBody.create(null, bodyp);

    const newStudyBlockId = uuidv4();

    // Create the study block and insert it at the found position
    const studyBlock = textSchema.nodes.generalStudyBlock.createChecked(
      { id: newStudyBlockId }, // Assign the generated ID
      [sbHeader, sbBody]
    );
    const tr = state.tr.insert(position, studyBlock);

    // Dispatch the transaction
    dispatch(tr);
    return true;
  }
};
