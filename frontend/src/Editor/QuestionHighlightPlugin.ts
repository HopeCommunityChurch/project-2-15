import {
  EditorState,
  Plugin,
  PluginKey,
  Transaction,
  EditorStateConfig,
  Selection,
  TextSelection,
} from "prosemirror-state";
import { EditorView, NodeView, DecorationSet, Decoration } from "prosemirror-view";
import { textSchema } from "./textSchema";
import { Slice, Node, Mark, Fragment } from "prosemirror-model";
import * as classes from "./styles.module.scss";

let questionHighlightKey = new PluginKey("questionHighlight");

export const questionHighlightPlugin = new Plugin({
  key: questionHighlightKey,
  state: {
    init: () => null,
    apply: (tr, oldState, _, st) => {
      const meta = tr.getMeta(questionHighlightKey);
      if (!meta) return oldState;
      const {action, qId} = meta;
      switch (action) {
        case "remove":
          return null;
        case "add":
          return qId;
      }
    }
  },
  props: {
    decorations: (st) => {
      const qId = questionHighlightKey.getState(st);
      if(qId == null) return DecorationSet.empty;
      const decorations = [];
      st.doc.descendants((node: Node, pos: number) => {
        if (node.type.name === "chunk") {
          return true;
        }
        if (node.type.name === "bibleText") {
          return true;
        }
        if (node.type.name === "section") {
          return true;
        }
        const m = node.marks.find((mark) => mark.attrs.questionId == qId)
        if(m) {
          decorations.push(
            Decoration.inline(pos, pos + node.nodeSize, {
              class: classes.referenceToPopOpen
            })
          )
        }
      });
      return DecorationSet.create(st.doc, decorations)
    },
  },
});


export function highlighQuestion (
  qId: string,
  state: EditorState,
  dispatch: (tr: Transaction) => void
) {
  dispatch(state.tr.setMeta(questionHighlightKey, {action: "add", qId}));
};

export function unhighlighQuestion (
  qId: string,
  state: EditorState,
  dispatch: (tr: Transaction) => void
) {
  dispatch(state.tr.setMeta(questionHighlightKey, {action: "remove", qId}));
};
