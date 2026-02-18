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
// Sink (indent) — only the current item, not its nested children
// ---------------------------------------------------------------------------

// Custom sink that, when the current list item has nested sub-lists,
// only indents the current line's paragraph. The nested children are
// placed as siblings of the sunk item in the new sub-list so they
// maintain their visual indentation level.
//
// Example — cursor on "B", press Tab:
//   Before:              After:
//   - A                  - A
//   - B        →           - B
//     - C                  - C
//       - D                  - D
//
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

  // Need a previous sibling to sink into
  if (itemIndex === 0) return false;

  // No nested children — use standard sink
  if (listItem.childCount <= 1) {
    return pmSinkListItem(listItemType)(state, dispatch);
  }

  if (!dispatch) return true;

  const prevItem = list.child(itemIndex - 1);
  const listType = list.type;
  const para = listItem.firstChild!;

  // Collect the nested sub-lists' items from the current item.
  // These will become siblings of the sunk item in the new sub-list,
  // preserving their visual indentation level.
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

  // Build the new previous item. If it already has a trailing sub-list
  // of the same type, merge into it instead of creating a second one.
  const prevChildren: any[] = [];
  for (let i = 0; i < prevItem.childCount; i++) {
    prevChildren.push(prevItem.child(i));
  }

  const lastPrevChild = prevChildren.length > 0 ? prevChildren[prevChildren.length - 1] : null;
  if (lastPrevChild && lastPrevChild.type === listType) {
    // Merge: append new items to the existing sub-list
    const existingItems: any[] = [];
    for (let i = 0; i < lastPrevChild.childCount; i++) {
      existingItems.push(lastPrevChild.child(i));
    }
    prevChildren[prevChildren.length - 1] = listType.create(
      lastPrevChild.attrs,
      Fragment.from([...existingItems, ...newItems])
    );
  } else {
    // No existing sub-list — create a new one
    prevChildren.push(listType.create(null, newItems));
  }

  const newPrevItem = listItemType.create(prevItem.attrs, Fragment.from(prevChildren));

  // Replace prevItem + currentItem with just newPrevItem
  const liStart = $from.before(liDepth);
  const liEnd = $from.after(liDepth);
  const prevItemStart = liStart - prevItem.nodeSize;
  const cursorOffset = $from.parentOffset;

  // Compute where the sunk paragraph ends up inside newPrevItem.
  // Walk through prevChildren to find the sub-list, then find sunkItem inside it.
  let offsetInNewItem = 1; // enter <li>
  for (let i = 0; i < prevChildren.length - 1; i++) {
    offsetInNewItem += prevChildren[i].nodeSize;
  }
  // Now at the start of the (possibly merged) sub-list
  offsetInNewItem += 1; // enter <ul>/<ol>
  if (lastPrevChild && lastPrevChild.type === listType) {
    // Merged: skip the existing items that were already in the sub-list
    for (let i = 0; i < lastPrevChild.childCount; i++) {
      offsetInNewItem += lastPrevChild.child(i).nodeSize;
    }
  }
  offsetInNewItem += 1; // enter sunkItem <li>
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

// For non-first list items: if the current paragraph is empty, lift the item
// out of the list. If non-empty, merge into the previous item's paragraph.
const joinListItems = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor || $cursor.parentOffset !== 0) return false;
  const depth = $cursor.depth;
  if (depth < 2) return false;
  if ($cursor.node(depth - 1).type !== state.schema.nodes.listItem) return false;
  // Only handle non-first items — first items are handled by listBackspace
  if ($cursor.index(depth - 2) === 0) return false;

  // Empty paragraph: lift item out of the list (remove bullet, keep the line).
  if ($cursor.parent.content.size === 0) {
    return pmLiftListItem(state.schema.nodes.listItem)(state, dispatch);
  }

  // Non-empty paragraph: merge into the previous item's paragraph.
  // Only handle the simple case: prev listItem has exactly one child (a paragraph).
  const list = $cursor.node(depth - 2);
  const prevListItem = list.child($cursor.index(depth - 2) - 1);
  if (prevListItem.childCount !== 1) return false;
  if (prevListItem.firstChild!.type !== state.schema.nodes.paragraph) return false;

  if (dispatch) {
    // Use resolved positions instead of magic token counts.
    // We need to delete from end-of-prev-paragraph-content to start-of-cur-paragraph-content,
    // which removes the boundary: </p></li><li><p>
    const prevParaContentEnd = $cursor.before(depth) - 2; // end of prev para content = before(<li>) - 1 (</p>)
    // More robustly: resolve structurally
    const curListItemStart = $cursor.before(depth - 1); // position before current <li>
    const prevListItemEnd = curListItemStart; // end of previous </li> is the same position
    // The previous listItem's last child (paragraph) content ends at:
    // prevListItemEnd - 1 (for </li>) - 1 (for </p>) = prevListItemEnd - 2
    // But let's compute it properly from the previous item:
    const prevItemEndPos = curListItemStart; // after prev </li>
    const prevItemStartPos = prevItemEndPos - prevListItem.nodeSize;
    const prevPara = prevListItem.firstChild!;
    // prevPara content ends at: prevItemStartPos + 1 (enter <li>) + prevPara.nodeSize - 1 (before </p>)
    const prevParaEnd = prevItemStartPos + 1 + prevPara.nodeSize - 1;

    const curParaContentStart = $cursor.pos;

    const tr = state.tr.delete(prevParaEnd, curParaContentStart);
    dispatch(tr.scrollIntoView());
  }
  return true;
};

// For first list items: lift the item out of the list (convert to plain paragraph).
const listBackspace = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor || $cursor.parentOffset !== 0) return false;
  const depth = $cursor.depth;
  if (depth < 2) return false;
  if ($cursor.node(depth - 1).type !== state.schema.nodes.listItem) return false;
  // Only fire for the first item — non-first items handled by joinListItems
  if ($cursor.index(depth - 2) !== 0) return false;
  return pmLiftListItem(state.schema.nodes.listItem)(state, dispatch);
};

// ---------------------------------------------------------------------------
// Delete commands
// ---------------------------------------------------------------------------

// For Delete key: merge the next list item's paragraph text into the current
// paragraph. Any sub-lists the consumed item had are adopted by the current item
// (Case A) or promoted into the parent list (Case B).
//
// Case A: cursor at end of a listItem's paragraph, next sibling listItem exists.
//   - A|       →  - AB
//   - B              - C
//     - C
//
// Case B: cursor at end of a non-list paragraph, next sibling is a list.
//   A|         →  AB
//   - B             - C
//     - C
//
const deleteIntoList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor) return false;
  if ($cursor.parentOffset !== $cursor.parent.content.size) return false;

  const depth = $cursor.depth;
  const schema = state.schema;
  const listItemType = schema.nodes.listItem;
  const curParaContentEnd = $cursor.pos;

  // Case A: inside a listItem, cursor in the paragraph which is the last child,
  // and there is a next sibling listItem. Merge next sibling's text, adopt its children.
  if (depth >= 2 && $cursor.node(depth - 1).type === listItemType) {
    const curItem = $cursor.node(depth - 1);
    const paraIndexInItem = $cursor.index(depth - 1);

    // Only handle when paragraph is the last child of the listItem.
    // If there's a sub-list after the paragraph, fall through to Case B.
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

        // Collect all children: merged paragraph, current item's sub-lists, next item's sub-lists
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
    // Paragraph is not the last child — fall through to Case B which handles
    // "paragraph followed by a list" within any container, including a listItem.
  }

  // Case B: cursor at end of a paragraph whose next sibling is a list.
  // Works both for paragraphs inside a listItem (sub-list after paragraph)
  // and for top-level paragraphs followed by a list.
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

  // Collect what replaces the first item in the list: its sub-lists' items
  // get promoted to siblings in the parent list.
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
    // Single item, no children — remove the whole list
    tr = tr.delete(listStart, listStart + nextSibling.nodeSize);
  } else {
    // Rebuild the list: promoted items + remaining siblings
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

  // Insert the consumed paragraph's content at cursor
  tr = tr.insert(curParaContentEnd, firstPara.content);
  tr = tr.setSelection(TextSelection.create(tr.doc, curParaContentEnd));
  dispatch(tr.scrollIntoView());
  return true;
};

// Handles cursor at start of a paragraph immediately following a list.
// Merges the paragraph's content into the last list item's paragraph,
// or deletes it if empty.
const joinAfterList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor || $cursor.parentOffset !== 0) return false;

  const depth = $cursor.depth;
  const schema = state.schema;
  // Not in a list item (listBackspace handles that case)
  if (depth >= 1 && $cursor.node(depth - 1).type === schema.nodes.listItem) return false;

  const indexInParent = $cursor.index(depth - 1);
  if (indexInParent === 0) return false;

  const parent = $cursor.node(depth - 1);
  const prevSibling = parent.child(indexInParent - 1);
  if (!isListNode(schema, prevSibling.type)) return false;

  const lastItem = prevSibling.lastChild;
  if (!lastItem || lastItem.type !== schema.nodes.listItem) return false;

  // Empty paragraph: just delete it, place cursor at end of previous content.
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

  // Find the deepest last paragraph in the last item's tree. If the last
  // item has nested sub-lists, walk into them to find the final paragraph.
  let targetPara = lastItem.firstChild;
  if (!targetPara || targetPara.type !== schema.nodes.paragraph) return false;
  let targetItem = lastItem;
  // Walk into nested lists: if the lastItem has a trailing sub-list,
  // descend into its last item recursively.
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
    // Use doc.resolve to find the end of targetPara's content.
    // The list ends right before the current paragraph starts.
    const curParaStart = $cursor.before(depth);
    // Resolve just inside the end of the list to find targetPara structurally.
    // The position curParaStart - 1 is inside the list's closing token boundary.
    // Search backward from there for the nearest valid text position, which will
    // be at the end of targetPara's content.
    const targetParaEnd = TextSelection.near(state.doc.resolve(curParaStart - 1), -1).$head.pos;

    const tr = state.tr.delete(targetParaEnd, curParaContentStart);
    dispatch(tr.scrollIntoView());
  }
  return true;
};

// ---------------------------------------------------------------------------
// Toggle commands (exposed for toolbar buttons)
// ---------------------------------------------------------------------------

// Convert the nearest ancestor list to a different type using setNodeMarkup,
// preserving nesting in a single transaction.
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

// Creates the keymap and input rules for list editing.
// `extraBackspace` is an optional array of commands to prepend to the
// Backspace chain (e.g. deleteQuestionSelection, deleteAnswerSelection
// in the main editor).
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
