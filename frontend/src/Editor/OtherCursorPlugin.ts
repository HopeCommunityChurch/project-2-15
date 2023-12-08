import {
  EditorState,
  Plugin,
  PluginKey,
  Transaction,
} from "prosemirror-state";
import { DecorationSet, Decoration } from "prosemirror-view";
import * as classes from "./styles.module.scss";

type Selection = {
  head: number;
  anchor: number;
};

let otherCursorKey = new PluginKey<Selection>("OtherCursor");

function makeWidget () {
  const elem = document.createElement("div");
  elem.className = classes.otherCursorHolder;

  const cursor = document.createElement("div");
  cursor.className = classes.otherCursor;
  elem.appendChild(cursor)
  return elem;
}

export const otherCursorPlugin = new Plugin<Selection>({
  key: otherCursorKey,
  state: {
    init: () => null,
    apply: (tr, st, _, _2) => {
      const meta = tr.getMeta(otherCursorKey);
      if (!meta) return st;
      const selection : Selection = meta;
      return selection;
    }
  },
  props: {
    decorations: (st) => {
      const selection = otherCursorKey.getState(st);
      if(selection == null) return DecorationSet.empty;
      const {head, anchor} = selection;
      const decorations = [
        Decoration.inline(anchor, head, {
          class: classes.otherSelection
        }),
        Decoration.widget(head, makeWidget),
      ];
      return DecorationSet.create(st.doc, decorations);
    },
  },
});


export function setSelection (
  selection: Selection,
  state: EditorState,
  dispatch: (tr: Transaction) => void
) {
  dispatch(state.tr.setMeta(otherCursorKey, selection));
};
