import {
  EditorState,
  Plugin,
  PluginKey,
  Transaction,
} from "prosemirror-state";
import { DecorationSet, Decoration } from "prosemirror-view";
import { Node } from "prosemirror-model";

let questionHighlightKey = new PluginKey<QuestionId []>("questionHighlight");

type QuestionId = Text

export const questionHighlightPlugin = new Plugin<QuestionId []>({
  key: questionHighlightKey,
  state: {
    init: () => [],
    apply: (tr, st, _, _2) => {
      const meta = tr.getMeta(questionHighlightKey);
      if (!meta) return st;
      const {action, qId} = meta;
      switch (action) {
        case "remove":
          return st.filter( (id) => id != qId);
        case "add":
          st.push(qId);
          return st;
      }
    }
  },
  props: {
    decorations: (st) => {
      const qIds = questionHighlightKey.getState(st);
      if(qIds.length === 0) return DecorationSet.empty;
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
        const m = node.marks.find((mark) => {
          return qIds.some( (qId) => mark.attrs.questionId == qId);
        })
        if(m) {
          decorations.push(
            Decoration.inline(pos, pos + node.nodeSize, {
              class: "referenceToPopOpen"
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
