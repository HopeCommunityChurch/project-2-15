import {
  EditorState,
  Plugin,
  PluginKey,
  Transaction,
} from "prosemirror-state";
import { DecorationSet, Decoration } from "prosemirror-view";

type Selection = {
  head: number;
  anchor: number;
};

let otherCursorKey = new PluginKey<Selection>("OtherCursor");

function makeWidget () {
  const elem = document.createElement("div");
  elem.className = "otherCursorHolder";
  elem.setAttribute("contenteditable", "false");

  const cursor = document.createElement("div");
  cursor.className = "otherCursor";
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
      const p1 = (anchor <= head)? anchor : head;
      const p2 = (anchor <= head)? head : anchor;
      const decorations = [
        Decoration.inline(p1, p2, {
          class: "otherSelection"
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
