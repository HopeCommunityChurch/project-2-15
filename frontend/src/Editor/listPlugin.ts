import { EditorState, Transaction, TextSelection, Plugin } from "prosemirror-state";
import { Schema, NodeType, Fragment } from "prosemirror-model";
import { keymap } from "prosemirror-keymap";
import { chainCommands, joinForward, selectNodeForward, joinBackward, selectNodeBackward } from "prosemirror-commands";
import {
  wrapInList,
  splitListItem as pmSplitListItem,
  liftListItem as pmLiftListItem,
  sinkListItem as pmSinkListItem,
} from "prosemirror-schema-list";
import { inputRules, wrappingInputRule } from "prosemirror-inputrules";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function listNodeTypes(schema: Schema) {
  return [schema.nodes.bulletList, schema.nodes.orderedList];
}

function isListNode(schema: Schema, type: NodeType): boolean {
  return listNodeTypes(schema).includes(type);
}

function isInList(state: EditorState, listType: NodeType): boolean {
  const { $from } = state.selection;
  for (let d = $from.depth; d >= 0; d--) {
    if ($from.node(d).type === listType) return true;
  }
  return false;
}

function findAncestorList(state: EditorState): NodeType | null {
  const { $from } = state.selection;
  for (let d = $from.depth; d >= 0; d--) {
    if (isListNode(state.schema, $from.node(d).type)) return $from.node(d).type;
  }
  return null;
}

// ---------------------------------------------------------------------------
// Sink (indent) — only the current item, not its children
// ---------------------------------------------------------------------------

// Standard sinkListItem moves children with the parent. This custom version
// keeps children at their visual indentation level:
//   - A                  - A
//   - B|       →           - B
//     - C                  - C
//       - D                  - D
const sinkCurrentItem = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const { $from } = state.selection;
  const schema = state.schema;
  const listItemType = schema.nodes.listItem;

  // Find the listItem containing the cursor
  let liDepth = -1;
  for (let d = $from.depth; d >= 0; d--) {
    if ($from.node(d).type === listItemType) {
      liDepth = d;
      break;
    }
  }
  if (liDepth < 1) return false;

  const listItem = $from.node(liDepth);
  const list = $from.node(liDepth - 1);
  const itemIndex = $from.index(liDepth - 1);

  if (itemIndex === 0) return false;

  // No nested children — standard sinkListItem suffices
  if (listItem.childCount <= 1) {
    return pmSinkListItem(listItemType)(state, dispatch);
  }

  if (!dispatch) return true;

  const prevItem = list.child(itemIndex - 1);
  const listType = list.type;
  const para = listItem.firstChild!;

  const siblingItems: any[] = [];
  for (let i = 1; i < listItem.childCount; i++) {
    const child = listItem.child(i);
    if (isListNode(schema, child.type)) {
      for (let j = 0; j < child.childCount; j++) {
        siblingItems.push(child.child(j));
      }
    }
  }

  const sunkItem = listItemType.create(null, para);
  const newItems = [sunkItem, ...siblingItems];

  // Merge into existing trailing sub-list if present, otherwise create one
  const prevChildren: any[] = [];
  for (let i = 0; i < prevItem.childCount; i++) {
    prevChildren.push(prevItem.child(i));
  }

  const lastPrevChild = prevChildren.length > 0 ? prevChildren[prevChildren.length - 1] : null;
  if (lastPrevChild && lastPrevChild.type === listType) {
    const existingItems: any[] = [];
    for (let i = 0; i < lastPrevChild.childCount; i++) {
      existingItems.push(lastPrevChild.child(i));
    }
    prevChildren[prevChildren.length - 1] = listType.create(
      lastPrevChild.attrs,
      Fragment.from([...existingItems, ...newItems])
    );
  } else {
    prevChildren.push(listType.create(null, newItems));
  }

  const newPrevItem = listItemType.create(prevItem.attrs, Fragment.from(prevChildren));

  const liStart = $from.before(liDepth);
  const liEnd = $from.after(liDepth);
  const prevItemStart = liStart - prevItem.nodeSize;
  const cursorOffset = $from.parentOffset;

  // Walk newPrevItem to find the cursor position inside the sunk paragraph
  let offsetInNewItem = 1;
  for (let i = 0; i < prevChildren.length - 1; i++) {
    offsetInNewItem += prevChildren[i].nodeSize;
  }
  offsetInNewItem += 1; // enter sub-list
  if (lastPrevChild && lastPrevChild.type === listType) {
    for (let i = 0; i < lastPrevChild.childCount; i++) {
      offsetInNewItem += lastPrevChild.child(i).nodeSize;
    }
  }
  offsetInNewItem += 1; // enter <li>
  offsetInNewItem += 1; // enter <p>

  const newCursorPos = prevItemStart + offsetInNewItem + cursorOffset;

  const tr = state.tr.replaceWith(prevItemStart, liEnd, newPrevItem);
  tr.setSelection(TextSelection.create(tr.doc, newCursorPos));
  dispatch(tr.scrollIntoView());
  return true;
};

// ---------------------------------------------------------------------------
// Backspace commands
// ---------------------------------------------------------------------------

// Backspace on non-first list items: empty → lift out, non-empty → merge into previous
const joinListItems = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor || $cursor.parentOffset !== 0) return false;
  const depth = $cursor.depth;
  if (depth < 2) return false;
  if ($cursor.node(depth - 1).type !== state.schema.nodes.listItem) return false;
  if ($cursor.index(depth - 2) === 0) return false; // first items handled by listBackspace

  if ($cursor.parent.content.size === 0) {
    return pmLiftListItem(state.schema.nodes.listItem)(state, dispatch);
  }

  // Only merge when prev item is simple (single paragraph child)
  const list = $cursor.node(depth - 2);
  const prevListItem = list.child($cursor.index(depth - 2) - 1);
  if (prevListItem.childCount !== 1) return false;
  if (prevListItem.firstChild!.type !== state.schema.nodes.paragraph) return false;

  if (dispatch) {
    // Delete from end of prev paragraph content to start of current, removing </p></li><li><p>
    const curListItemStart = $cursor.before(depth - 1);
    const prevItemStartPos = curListItemStart - prevListItem.nodeSize;
    const prevPara = prevListItem.firstChild!;
    const prevParaEnd = prevItemStartPos + 1 + prevPara.nodeSize - 1;

    const curParaContentStart = $cursor.pos;

    const tr = state.tr.delete(prevParaEnd, curParaContentStart);
    dispatch(tr.scrollIntoView());
  }
  return true;
};

// Backspace on first list item: lift out of list
const listBackspace = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor || $cursor.parentOffset !== 0) return false;
  const depth = $cursor.depth;
  if (depth < 2) return false;
  if ($cursor.node(depth - 1).type !== state.schema.nodes.listItem) return false;
  if ($cursor.index(depth - 2) !== 0) return false; // non-first items handled by joinListItems
  return pmLiftListItem(state.schema.nodes.listItem)(state, dispatch);
};

// ---------------------------------------------------------------------------
// Delete commands
// ---------------------------------------------------------------------------

// Delete key: merge next item's text into current, adopting its children.
//   Case A: - A| + - B  →  - AB       Case B: A| + - B  →  AB
//             - C             - C               - C          - C
const deleteIntoList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor) return false;
  if ($cursor.parentOffset !== $cursor.parent.content.size) return false;

  const depth = $cursor.depth;
  const schema = state.schema;
  const listItemType = schema.nodes.listItem;
  const curParaContentEnd = $cursor.pos;

  // Case A: inside a listItem, next sibling listItem exists
  if (depth >= 2 && $cursor.node(depth - 1).type === listItemType) {
    const curItem = $cursor.node(depth - 1);
    const paraIndexInItem = $cursor.index(depth - 1);

    // Only when paragraph is last child; sub-list after paragraph falls through to Case B
    if (paraIndexInItem === curItem.childCount - 1) {
      const list = $cursor.node(depth - 2);
      const listItemIndex = $cursor.index(depth - 2);
      if (listItemIndex < list.childCount - 1) {
        const nextItem = list.child(listItemIndex + 1);
        if (!nextItem.firstChild || nextItem.firstChild.type !== schema.nodes.paragraph) return false;

        if (!dispatch) return true;

        const curPara = curItem.firstChild!;
        const nextPara = nextItem.firstChild!;

        const mergedPara = schema.nodes.paragraph.create(
          curPara.attrs,
          curPara.content.append(nextPara.content)
        );

        const children: any[] = [mergedPara];
        for (let i = 1; i < curItem.childCount; i++) children.push(curItem.child(i));
        for (let i = 1; i < nextItem.childCount; i++) children.push(nextItem.child(i));

        const newItem = listItemType.create(curItem.attrs, Fragment.from(children));

        const curItemStart = $cursor.before(depth - 1);
        const nextItemEnd = curItemStart + curItem.nodeSize + nextItem.nodeSize;
        const tr = state.tr.replaceWith(curItemStart, nextItemEnd, newItem);
        tr.setSelection(TextSelection.create(tr.doc, curParaContentEnd));
        dispatch(tr.scrollIntoView());
        return true;
      }
    }
  }

  // Case B: paragraph followed by a list (works in any container)
  const container = $cursor.node(depth - 1);
  const indexInParent = $cursor.index(depth - 1);
  if (indexInParent >= container.childCount - 1) return false;
  const nextSibling = container.child(indexInParent + 1);
  if (!isListNode(schema, nextSibling.type)) return false;
  const firstItem = nextSibling.firstChild;
  if (!firstItem || firstItem.type !== listItemType) return false;
  const firstPara = firstItem.firstChild;
  if (!firstPara || firstPara.type !== schema.nodes.paragraph) return false;

  if (!dispatch) return true;

  const promotedItems: any[] = [];
  for (let i = 1; i < firstItem.childCount; i++) {
    const child = firstItem.child(i);
    if (isListNode(schema, child.type)) {
      for (let j = 0; j < child.childCount; j++) {
        promotedItems.push(child.child(j));
      }
    }
  }

  let tr = state.tr;
  const listStart = $cursor.after(depth);

  if (nextSibling.childCount === 1 && promotedItems.length === 0) {
    tr = tr.delete(listStart, listStart + nextSibling.nodeSize);
  } else {
    const newListItems: any[] = [...promotedItems];
    for (let i = 1; i < nextSibling.childCount; i++) {
      newListItems.push(nextSibling.child(i));
    }
    if (newListItems.length > 0) {
      const newList = nextSibling.type.create(nextSibling.attrs, Fragment.from(newListItems));
      tr = tr.replaceWith(listStart, listStart + nextSibling.nodeSize, newList);
    } else {
      tr = tr.delete(listStart, listStart + nextSibling.nodeSize);
    }
  }

  tr = tr.insert(curParaContentEnd, firstPara.content);
  tr = tr.setSelection(TextSelection.create(tr.doc, curParaContentEnd));
  dispatch(tr.scrollIntoView());
  return true;
};

// Backspace on paragraph after a list: merge into the deepest last list item
const joinAfterList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor || $cursor.parentOffset !== 0) return false;

  const depth = $cursor.depth;
  const schema = state.schema;
  if (depth >= 1 && $cursor.node(depth - 1).type === schema.nodes.listItem) return false;

  const indexInParent = $cursor.index(depth - 1);
  if (indexInParent === 0) return false;

  const parent = $cursor.node(depth - 1);
  const prevSibling = parent.child(indexInParent - 1);
  if (!isListNode(schema, prevSibling.type)) return false;

  const lastItem = prevSibling.lastChild;
  if (!lastItem || lastItem.type !== schema.nodes.listItem) return false;

  if ($cursor.parent.content.size === 0) {
    if (dispatch) {
      const curParaStart = $cursor.before(depth);
      const curParaEnd = $cursor.after(depth);
      const tr = state.tr.delete(curParaStart, curParaEnd);
      const sel = TextSelection.near(tr.doc.resolve(curParaStart), -1);
      tr.setSelection(sel);
      dispatch(tr.scrollIntoView());
    }
    return true;
  }

  // Walk nested sub-lists to find the deepest last paragraph
  let targetPara = lastItem.firstChild;
  if (!targetPara || targetPara.type !== schema.nodes.paragraph) return false;
  let targetItem = lastItem;
  let current = lastItem;
  while (current.lastChild && isListNode(schema, current.lastChild.type)) {
    const nestedList = current.lastChild;
    const nestedItem = nestedList.lastChild;
    if (!nestedItem || nestedItem.type !== schema.nodes.listItem) break;
    if (!nestedItem.firstChild || nestedItem.firstChild.type !== schema.nodes.paragraph) break;
    targetItem = nestedItem;
    targetPara = nestedItem.firstChild;
    current = nestedItem;
  }

  if (dispatch) {
    const curParaContentStart = $cursor.pos;
    const curParaStart = $cursor.before(depth);
    // Resolve backward from end of the list to find the target paragraph's content end
    const targetParaEnd = TextSelection.near(state.doc.resolve(curParaStart - 1), -1).$head.pos;

    const tr = state.tr.delete(targetParaEnd, curParaContentStart);
    dispatch(tr.scrollIntoView());
  }
  return true;
};

// ---------------------------------------------------------------------------
// Toggle commands (exposed for toolbar buttons)
// ---------------------------------------------------------------------------

// setNodeMarkup swaps the list type in-place, preserving nesting (preferred over lift+re-wrap)
function convertListType(state: EditorState, from: NodeType, to: NodeType, dispatch?: (tr: Transaction) => void): boolean {
  const { $from } = state.selection;
  for (let d = $from.depth; d >= 0; d--) {
    if ($from.node(d).type === from) {
      if (dispatch) {
        dispatch(state.tr.setNodeMarkup($from.before(d + 1), to).scrollIntoView());
      }
      return true;
    }
  }
  return false;
}

export const toggleBulletList = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const schema = state.schema;
  const currentList = findAncestorList(state);

  if (currentList === schema.nodes.bulletList) {
    return pmLiftListItem(schema.nodes.listItem)(state, dispatch);
  }
  if (currentList === schema.nodes.orderedList) {
    return convertListType(state, schema.nodes.orderedList, schema.nodes.bulletList, dispatch);
  }
  return wrapInList(schema.nodes.bulletList)(state, dispatch);
};

export const toggleOrderedList = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const schema = state.schema;
  const currentList = findAncestorList(state);

  if (currentList === schema.nodes.orderedList) {
    return pmLiftListItem(schema.nodes.listItem)(state, dispatch);
  }
  if (currentList === schema.nodes.bulletList) {
    return convertListType(state, schema.nodes.bulletList, schema.nodes.orderedList, dispatch);
  }
  return wrapInList(schema.nodes.orderedList)(state, dispatch);
};

// ---------------------------------------------------------------------------
// Input rules
// ---------------------------------------------------------------------------

function makeListInputRules(schema: Schema) {
  return inputRules({
    rules: [
      wrappingInputRule(/^[-*]\s$/, schema.nodes.bulletList),
      wrappingInputRule(
        /^(\d+)\.\s$/,
        schema.nodes.orderedList,
        (match: RegExpMatchArray) => ({ order: +match[1] }),
        (match: RegExpMatchArray, node: any) => node.childCount + node.attrs.order === +match[1]
      ),
      wrappingInputRule(
        /^(\d+)\)\s$/,
        schema.nodes.orderedList,
        (match: RegExpMatchArray) => ({ order: +match[1] }),
        (match: RegExpMatchArray, node: any) => node.childCount + node.attrs.order === +match[1]
      ),
    ],
  });
}

// ---------------------------------------------------------------------------
// Plugin factory
// ---------------------------------------------------------------------------

export function listPlugins(
  schema: Schema,
  extraBackspace: Array<(state: EditorState, dispatch?: (tr: Transaction) => void) => boolean> = []
): Plugin[] {
  const listItemType = schema.nodes.listItem;

  const backspaceChain = [
    ...extraBackspace,
    joinListItems,
    listBackspace,
    joinAfterList,
    joinBackward,
    selectNodeBackward,
  ];

  return [
    makeListInputRules(schema),
    keymap({
      Tab: chainCommands(sinkCurrentItem),
      "Shift-Tab": chainCommands(pmLiftListItem(listItemType)),
      Enter: pmSplitListItem(listItemType),
      Backspace: chainCommands(...backspaceChain),
      "Mod-Backspace": chainCommands(joinListItems, listBackspace, joinAfterList),
      Delete: chainCommands(deleteIntoList, joinForward, selectNodeForward),
    }),
  ];
}
