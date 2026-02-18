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
import { inputRules, wrappingInputRule, InputRule } from "prosemirror-inputrules";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function listNodeTypes(schema: Schema) {
  return [schema.nodes.bulletList, schema.nodes.orderedList, schema.nodes.checkList];
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

/** Returns the item type (listItem or checkListItem) for the item the cursor is in, or null */
function findItemType(state: EditorState): NodeType | null {
  const { $from } = state.selection;
  const schema = state.schema;
  for (let d = $from.depth; d >= 0; d--) {
    const t = $from.node(d).type;
    if (t === schema.nodes.listItem || t === schema.nodes.checkListItem) return t;
  }
  return null;
}

function isAnyListItem(schema: Schema, type: NodeType): boolean {
  return type === schema.nodes.listItem || type === schema.nodes.checkListItem;
}

/** Count text characters from doc start to `pos` (ignoring structural tokens) */
function docTextOffset(doc: any, pos: number): number {
  let chars = 0;
  let found = false;
  doc.nodesBetween(0, doc.content.size, (node: any, nodePos: number) => {
    if (found) return false;
    if (node.isText) {
      if (nodePos + node.text!.length >= pos) {
        chars += Math.max(0, pos - nodePos);
        found = true;
        return false;
      }
      chars += node.text!.length;
      return false;
    }
    return true;
  });
  return chars;
}

/** Find document position corresponding to a text character offset */
function posFromTextOffset(doc: any, targetChars: number): number {
  let chars = 0;
  let result = 0;
  let found = false;
  doc.nodesBetween(0, doc.content.size, (node: any, nodePos: number) => {
    if (found) return false;
    if (node.isText) {
      if (chars + node.text!.length >= targetChars) {
        result = nodePos + (targetChars - chars);
        found = true;
        return false;
      }
      chars += node.text!.length;
      return false;
    }
    return true;
  });
  return result;
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
export const sinkCurrentItem = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const { $from } = state.selection;
  const schema = state.schema;
  const itemType = findItemType(state);
  if (!itemType) return false;

  // Find the item containing the cursor
  let liDepth = -1;
  for (let d = $from.depth; d >= 0; d--) {
    if ($from.node(d).type === itemType) {
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
    return pmSinkListItem(itemType)(state, dispatch);
  }

  if (!dispatch) return true;

  const prevItem = list.child(itemIndex - 1);
  const listType = list.type;
  const para = listItem.firstChild!;

  // Only the paragraph sinks — children (sublists) stay at their visual level
  const sunkItem = itemType.create(itemType === schema.nodes.checkListItem ? { checked: false } : null, para);

  // Collect the item's children (sublists) to place as siblings in prev item
  const itemChildLists: any[] = [];
  for (let i = 1; i < listItem.childCount; i++) {
    itemChildLists.push(listItem.child(i));
  }

  // Merge sunk item into existing trailing sub-list if present, otherwise create one
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
      Fragment.from([...existingItems, sunkItem])
    );
  } else {
    prevChildren.push(listType.create(null, Fragment.from([sunkItem])));
  }

  // Append item's children (sublists) as siblings in the prev item, preserving their types
  for (const child of itemChildLists) {
    prevChildren.push(child);
  }

  const newPrevItem = itemType.create(prevItem.attrs, Fragment.from(prevChildren));

  const liStart = $from.before(liDepth);
  const liEnd = $from.after(liDepth);
  const prevItemStart = liStart - prevItem.nodeSize;
  const cursorOffset = $from.parentOffset;

  // Walk newPrevItem to find the cursor position inside the sunk paragraph
  // The nested list containing our sunk item is right before the appended child lists
  const nestedListIndex = prevChildren.length - itemChildLists.length - 1;
  let offsetInNewItem = 1;
  for (let i = 0; i < nestedListIndex; i++) {
    offsetInNewItem += prevChildren[i].nodeSize;
  }
  const nestedListNode = prevChildren[nestedListIndex];
  offsetInNewItem += 1; // enter sub-list
  // Skip to the last item in the nested list (our sunk item)
  for (let i = 0; i < nestedListNode.childCount - 1; i++) {
    offsetInNewItem += nestedListNode.child(i).nodeSize;
  }
  offsetInNewItem += 1; // enter <li>
  offsetInNewItem += 1; // enter <p>

  const newCursorPos = prevItemStart + offsetInNewItem + cursorOffset;

  const tr = state.tr.replaceWith(prevItemStart, liEnd, newPrevItem);
  tr.setSelection(TextSelection.create(tr.doc, newCursorPos));
  dispatch(tr.scrollIntoView());
  return true;
};

// Sink into a sibling list: when at item index 0 and a list exists right before
// this list in the parent, move the item into the last item of that previous list.
//   [] asdf          [] asdf
//   1. first|    →     1. first|
//   2. second        1. second
export const sinkIntoSiblingList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const { $from } = state.selection;
  const schema = state.schema;
  const itemType = findItemType(state);
  if (!itemType) return false;

  let liDepth = -1;
  for (let d = $from.depth; d >= 0; d--) {
    if ($from.node(d).type === itemType) { liDepth = d; break; }
  }
  if (liDepth < 1) return false;

  const list = $from.node(liDepth - 1);
  const listDepth = liDepth - 1;
  const itemIndex = $from.index(listDepth);

  // Only handle first item — non-first items are handled by sinkCurrentItem
  if (itemIndex !== 0) return false;

  // Find the parent container and the index of our list in it
  if (listDepth < 1) return false;
  const container = $from.node(listDepth - 1);
  const listIndexInContainer = $from.index(listDepth - 1);
  if (listIndexInContainer === 0) return false;

  const prevSibling = container.child(listIndexInContainer - 1);
  if (!isListNode(schema, prevSibling.type)) return false;

  // The previous sibling is a list — we'll move our item into its last item
  const prevLastItem = prevSibling.lastChild!;
  if (!prevLastItem || !isAnyListItem(schema, prevLastItem.type)) return false;

  if (!dispatch) return true;

  const cursorOffset = $from.parentOffset;
  const item = list.child(0);
  const listType = list.type;

  // Only take the paragraph — children stay at their visual indentation level
  const nestedItem = itemType.create(
    itemType === schema.nodes.checkListItem ? { checked: false } : null,
    item.firstChild! // paragraph only
  );

  // Collect the item's children (sublists) — they'll become siblings in the prev item
  const itemChildren: any[] = [];
  for (let i = 1; i < item.childCount; i++) {
    itemChildren.push(item.child(i));
  }

  // Rebuild the previous list's last item:
  // existing content + nested list with our item + our item's children (sublists)
  const prevLastChildren: any[] = [];
  for (let i = 0; i < prevLastItem.childCount; i++) {
    prevLastChildren.push(prevLastItem.child(i));
  }

  // Merge into existing trailing sublist of same type, or create new one
  const lastPrevChild = prevLastChildren.length > 0 ? prevLastChildren[prevLastChildren.length - 1] : null;
  if (lastPrevChild && lastPrevChild.type === listType) {
    const merged: any[] = [];
    for (let j = 0; j < lastPrevChild.childCount; j++) merged.push(lastPrevChild.child(j));
    merged.push(nestedItem);
    prevLastChildren[prevLastChildren.length - 1] = listType.create(lastPrevChild.attrs, Fragment.from(merged));
  } else {
    prevLastChildren.push(listType.create(null, Fragment.from([nestedItem])));
  }

  // Append item's children (sublists) as siblings in the prev item
  for (const child of itemChildren) {
    prevLastChildren.push(child);
  }

  const newPrevLastItem = prevLastItem.type.create(prevLastItem.attrs, Fragment.from(prevLastChildren));

  // Rebuild the previous list with updated last item
  const prevListItems: any[] = [];
  for (let i = 0; i < prevSibling.childCount - 1; i++) prevListItems.push(prevSibling.child(i));
  prevListItems.push(newPrevLastItem);
  const newPrevList = prevSibling.type.create(prevSibling.attrs, Fragment.from(prevListItems));

  // Rebuild our list without the first item (or remove it entirely)
  const listPos = $from.start(listDepth) - 1;
  const prevListPos = listPos - prevSibling.nodeSize;

  let tr: Transaction;
  if (list.childCount === 1) {
    tr = state.tr.replaceWith(prevListPos, listPos + list.nodeSize, newPrevList);
  } else {
    const remainingItems: any[] = [];
    for (let i = 1; i < list.childCount; i++) remainingItems.push(list.child(i));
    const newList = list.type.create(list.attrs, Fragment.from(remainingItems));
    tr = state.tr.replaceWith(prevListPos, listPos + list.nodeSize, Fragment.from([newPrevList, newList]));
  }

  // Position cursor inside the sunk item's paragraph
  let pos = prevListPos + 1; // enter prev list
  for (let i = 0; i < prevListItems.length - 1; i++) pos += prevListItems[i].nodeSize;
  pos += 1; // enter last item
  // Walk to the nested list containing our item (skip earlier children of prev item)
  const nestedListIndex = prevLastChildren.length - 1 - itemChildren.length;
  for (let i = 0; i < nestedListIndex; i++) pos += prevLastChildren[i].nodeSize;
  const nestedListNode = prevLastChildren[nestedListIndex];
  pos += 1; // enter nested list
  // Skip to last item in the nested list (our item is last)
  for (let i = 0; i < nestedListNode.childCount - 1; i++) pos += nestedListNode.child(i).nodeSize;
  pos += 1; // enter item
  pos += 1; // enter paragraph
  pos += cursorOffset;

  tr.setSelection(TextSelection.create(tr.doc, pos));
  dispatch(tr.scrollIntoView());
  return true;
};

// Lift from nested list: when a list item is inside a list nested inside
// another list item, lift it out to become a sibling of the parent item
// (converting its type to match the parent list if needed).
//   checkListItem              checkListItem
//     orderedList         →      orderedList
//       listItem "first"           listItem "first"
//       listItem "second"←     checkListItem "second"  ← now a sibling
const liftFromNestedList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const { $from } = state.selection;
  const schema = state.schema;
  const itemType = findItemType(state);
  if (!itemType) return false;

  // Find our item depth and its parent list
  let liDepth = -1;
  for (let d = $from.depth; d >= 0; d--) {
    if ($from.node(d).type === itemType) { liDepth = d; break; }
  }
  if (liDepth < 1) return false;

  const innerList = $from.node(liDepth - 1);
  const innerListDepth = liDepth - 1;
  if (!isListNode(schema, innerList.type)) return false;

  // Check if the inner list is inside another list item
  if (innerListDepth < 1) return false;
  const outerItem = $from.node(innerListDepth - 1);
  if (!isAnyListItem(schema, outerItem.type)) return false;

  // Check there's an outer list above the outer item
  const outerItemDepth = innerListDepth - 1;
  if (outerItemDepth < 1) return false;
  const outerList = $from.node(outerItemDepth - 1);
  if (!isListNode(schema, outerList.type)) return false;

  // We have: outerList > outerItem > innerList > item
  const outerItemType = outerItem.type;

  if (!dispatch) return true;

  const cursorOffset = $from.parentOffset;
  const itemIndex = $from.index(innerListDepth);
  const item = innerList.child(itemIndex);

  // Only lift the paragraph — children (sublists) stay at their visual level in the outer item
  // Keep the item's original type (matching its inner list)
  const liftedPara = item.firstChild!;
  const newItem = itemType.create(
    itemType === schema.nodes.checkListItem ? { checked: item.attrs.checked ?? false } : null,
    Fragment.from([liftedPara])
  );

  // Collect item's children (sublists) to keep in the outer item
  const itemChildLists: any[] = [];
  for (let i = 1; i < item.childCount; i++) {
    itemChildLists.push(item.child(i));
  }

  // Rebuild: split innerList around the item, rebuild outerItem, split outerList
  const outerListDepth = outerItemDepth - 1;
  const outerListPos = $from.start(outerListDepth) - 1;
  const outerItemIndex = $from.index(outerListDepth);

  // Split outerItem children into: before innerList, after innerList
  // "After" nodes become children of the lifted item (they were its children before indent)
  const outerItemBefore: any[] = [];
  const outerItemAfter: any[] = [];
  let foundInnerList = false;
  let innerListChildIndex = -1;
  for (let i = 0; i < outerItem.childCount; i++) {
    const child = outerItem.child(i);
    if (child === innerList) {
      foundInnerList = true;
      innerListChildIndex = i;
      // Items before current in the innerList stay in outerItem
      if (itemIndex > 0) {
        const before: any[] = [];
        for (let j = 0; j < itemIndex; j++) before.push(innerList.child(j));
        outerItemBefore.push(innerList.type.create(innerList.attrs, Fragment.from(before)));
      }
      // Items after current in the innerList become children of the lifted item
      // (they were siblings, now they nest under the lifted item)
    } else if (!foundInnerList) {
      outerItemBefore.push(child);
    } else {
      outerItemAfter.push(child);
    }
  }

  // Collect items after current in the innerList — they become a nested list under the lifted item
  const innerListAfterItems: any[] = [];
  for (let j = itemIndex + 1; j < innerList.childCount; j++) innerListAfterItems.push(innerList.child(j));

  // Item's own children + inner list siblings after + nodes from outerItem that were after the innerList
  // all become children of the lifted item
  const liftedChildren: any[] = [liftedPara];
  for (const cl of itemChildLists) liftedChildren.push(cl);
  if (innerListAfterItems.length > 0) {
    liftedChildren.push(innerList.type.create(innerList.attrs, Fragment.from(innerListAfterItems)));
  }
  for (const cl of outerItemAfter) liftedChildren.push(cl);

  const liftedItemContent = itemType.create(
    itemType === schema.nodes.checkListItem ? { checked: item.attrs.checked ?? false } : null,
    Fragment.from(liftedChildren)
  );

  const modifiedOuterItem = outerItemType.create(outerItem.attrs, Fragment.from(outerItemBefore));

  // Convert lifted item type if needed
  let liftedItem = liftedItemContent;
  if (innerList.type !== outerList.type && itemType !== outerItemType) {
    const convertAttrs = outerItemType === schema.nodes.checkListItem ? { checked: false } : null;
    liftedItem = outerItemType.create(convertAttrs, liftedItemContent.content, liftedItemContent.marks);
  } else if (itemType !== outerItemType) {
    const convertAttrs = outerItemType === schema.nodes.checkListItem ? { checked: false } : null;
    liftedItem = outerItemType.create(convertAttrs, liftedItemContent.content, liftedItemContent.marks);
  }

  // Insert lifted item into outer list (converting type to match outer list)
  const outerListItems: any[] = [];
  for (let i = 0; i <= outerItemIndex; i++) {
    if (i === outerItemIndex) {
      outerListItems.push(modifiedOuterItem);
    } else {
      outerListItems.push(outerList.child(i));
    }
  }
  outerListItems.push(liftedItem);
  for (let i = outerItemIndex + 1; i < outerList.childCount; i++) {
    outerListItems.push(outerList.child(i));
  }
  const newOuterList = outerList.type.create(outerList.attrs, Fragment.from(outerListItems));
  const tr = state.tr.replaceWith(outerListPos, outerListPos + outerList.nodeSize, newOuterList);

  // Cursor: skip to the lifted item (right after modified outer item)
  let pos = outerListPos + 1; // enter outer list
  for (let i = 0; i <= outerItemIndex; i++) {
    pos += (i === outerItemIndex ? modifiedOuterItem : outerList.child(i)).nodeSize;
  }
  pos += 1; // enter lifted item
  pos += 1; // enter paragraph
  pos += cursorOffset;

  tr.setSelection(TextSelection.create(tr.doc, pos));
  dispatch(tr.scrollIntoView());
  return true;
};

export const liftCurrentItem = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const itemType = findItemType(state);
  if (!itemType) return false;
  // Try nested lift first, then standard lift
  if (liftFromNestedList(state, dispatch)) return true;
  return pmLiftListItem(itemType)(state, dispatch);
};

// ---------------------------------------------------------------------------
// Range-aware indent/outdent for multi-select
// ---------------------------------------------------------------------------

/** Collect list items touched by the selection, based on which paragraphs are in range.
 *  This avoids picking up ancestor list items that merely contain the selection. */
function collectListItemsInSelection(state: EditorState): { pos: number; node: any; depth: number }[] {
  const { from, to } = state.selection;
  const schema = state.schema;
  const seen = new Set<number>();
  const items: { pos: number; node: any; depth: number }[] = [];

  state.doc.nodesBetween(from, to, (node, pos) => {
    if (node.type === schema.nodes.paragraph) {
      // Walk up to find the nearest list item ancestor
      const $pos = state.doc.resolve(pos);
      for (let d = $pos.depth; d >= 0; d--) {
        if (isAnyListItem(schema, $pos.node(d).type)) {
          const itemPos = $pos.before(d);
          if (!seen.has(itemPos)) {
            seen.add(itemPos);
            items.push({ pos: itemPos, node: $pos.node(d), depth: d });
          }
          break;
        }
      }
      return false; // don't descend into paragraph
    }
    return true;
  });

  return items;
}

/** Range-aware sink: indent all selected list items, or none if any can't be indented.
 *  Only activates for multi-item selections (2+ top-level items).
 *  Sibling items in the same list are moved as a batch so they don't double-indent. */
export const sinkSelectedItems = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  if (state.selection.from === state.selection.to) return false;
  const items = collectListItemsInSelection(state);
  if (items.length === 0) return false;

  const schema = state.schema;

  // Filter to shallowest selected items (not nested inside other selected items)
  const selectedPositions = new Set(items.map(i => i.pos));
  const topItems = items.filter(item => {
    const $pos = state.doc.resolve(item.pos);
    for (let d = $pos.depth - 1; d >= 0; d--) {
      if (isAnyListItem(schema, $pos.node(d).type)) {
        const ancestorPos = $pos.before(d);
        if (selectedPositions.has(ancestorPos)) return false;
      }
    }
    return true;
  });

  if (topItems.length < 2) return false;

  // Group topItems by parent list position
  const byList = new Map<number, { items: typeof topItems; listDepth: number }>();

  for (const item of topItems) {
    const $pos = state.doc.resolve(item.pos);
    let listDepth = -1;
    for (let d = $pos.depth; d >= 0; d--) {
      if (isListNode(schema, $pos.node(d).type)) { listDepth = d; break; }
    }
    if (listDepth < 0) return false;
    const listPos = $pos.start(listDepth) - 1;
    if (!byList.has(listPos)) byList.set(listPos, { items: [], listDepth });
    byList.get(listPos)!.items.push(item);
  }

  // For each list, find the target (preceding unselected sibling) and check feasibility
  for (const [listPos, { items: listItems, listDepth }] of byList) {
    const topItemPositions = new Set(topItems.map(i => i.pos));

    for (const item of listItems) {
      const $pos = state.doc.resolve(item.pos);
      const itemIndex = $pos.index(listDepth);
      const parentList = $pos.node(listDepth);

      // Find the nearest preceding unselected sibling
      let targetIndex = -1;
      for (let i = itemIndex - 1; i >= 0; i--) {
        // Check if this sibling is selected (in topItems for this list)
        let siblingPos = $pos.start(listDepth);
        for (let j = 0; j < i; j++) siblingPos += parentList.child(j).nodeSize;
        if (!topItemPositions.has(siblingPos)) {
          targetIndex = i;
          break;
        }
      }

      if (targetIndex < 0) {
        // No preceding unselected sibling — check for sibling list in container
        if (itemIndex === 0 || targetIndex < 0) {
          if (listDepth < 1) return false;
          const container = $pos.node(listDepth - 1);
          const listIndexInContainer = $pos.index(listDepth - 1);
          if (listIndexInContainer === 0) return false;
          const prevSibling = container.child(listIndexInContainer - 1);
          if (!isListNode(schema, prevSibling.type)) return false;
        }
      }
    }
  }

  if (!dispatch) return true;

  const fromTextOff = docTextOffset(state.doc, state.selection.from);
  const toTextOff = docTextOffset(state.doc, state.selection.to);

  // Process lists bottom-to-top by list position
  const sortedListPositions = [...byList.keys()].sort((a, b) => b - a);
  let tr = state.tr;

  for (const origListPos of sortedListPositions) {
    const { items: listItems, listDepth } = byList.get(origListPos)!;
    const mappedListPos = tr.mapping.map(origListPos);
    const list = tr.doc.nodeAt(mappedListPos);
    if (!list || !isListNode(schema, list.type)) continue;

    // Get selected item indices in this list
    const topItemPositions = new Set(topItems.map(i => tr.mapping.map(i.pos)));
    const selectedIndices = new Set<number>();
    let offset = mappedListPos + 1;
    for (let i = 0; i < list.childCount; i++) {
      if (topItemPositions.has(offset)) selectedIndices.add(i);
      offset += list.child(i).nodeSize;
    }

    // Group consecutive selected items by their target (nearest preceding unselected sibling)
    // Process groups from end to start
    const groups: { targetIndex: number; indices: number[] }[] = [];
    let currentGroup: { targetIndex: number; indices: number[] } | null = null;

    for (let i = 0; i < list.childCount; i++) {
      if (selectedIndices.has(i)) {
        // Find target: nearest preceding unselected index
        let target = -1;
        for (let j = i - 1; j >= 0; j--) {
          if (!selectedIndices.has(j)) { target = j; break; }
        }
        if (currentGroup && currentGroup.targetIndex === target) {
          currentGroup.indices.push(i);
        } else {
          if (currentGroup) groups.push(currentGroup);
          currentGroup = { targetIndex: target, indices: [i] };
        }
      }
    }
    if (currentGroup) groups.push(currentGroup);

    // Process groups from end to start (so positions stay valid)
    for (let g = groups.length - 1; g >= 0; g--) {
      const group = groups[g];
      const sunkItems: any[] = group.indices.map(i => list.child(i));

      if (group.targetIndex >= 0) {
        // Sink into the target item's trailing sublist
        const targetItem = list.child(group.targetIndex);
        let targetStart = mappedListPos + 1;
        for (let i = 0; i < group.targetIndex; i++) targetStart += list.child(i).nodeSize;

        let firstSunkStart = mappedListPos + 1;
        for (let i = 0; i < group.indices[0]; i++) firstSunkStart += list.child(i).nodeSize;
        const lastIdx = group.indices[group.indices.length - 1];
        let lastSunkEnd = mappedListPos + 1;
        for (let i = 0; i <= lastIdx; i++) lastSunkEnd += list.child(i).nodeSize;

        // Build new target item
        const targetChildren: any[] = [];
        for (let i = 0; i < targetItem.childCount; i++) targetChildren.push(targetItem.child(i));

        const lastTargetChild = targetChildren.length > 0 ? targetChildren[targetChildren.length - 1] : null;
        if (lastTargetChild && lastTargetChild.type === list.type) {
          const existing: any[] = [];
          for (let i = 0; i < lastTargetChild.childCount; i++) existing.push(lastTargetChild.child(i));
          targetChildren[targetChildren.length - 1] = list.type.create(lastTargetChild.attrs, Fragment.from([...existing, ...sunkItems]));
        } else {
          targetChildren.push(list.type.create(null, Fragment.from(sunkItems)));
        }

        const newTargetItem = targetItem.type.create(targetItem.attrs, Fragment.from(targetChildren));

        // Replace from target start to end of last sunk item (removing items between target and sunk if any)
        // But there may be unselected items between target and sunk items — keep those
        const replacementNodes: any[] = [newTargetItem];
        for (let i = group.targetIndex + 1; i < group.indices[0]; i++) {
          replacementNodes.push(list.child(i));
        }

        tr = tr.replaceWith(targetStart, lastSunkEnd, replacementNodes);
      } else {
        // No preceding unselected sibling — sink into previous sibling list in container
        const $listPos = tr.doc.resolve(mappedListPos);
        const container = $listPos.node($listPos.depth);
        const listIndexInContainer = $listPos.index($listPos.depth);
        const prevSiblingList = container.child(listIndexInContainer - 1);
        const prevListStart = mappedListPos - prevSiblingList.nodeSize;
        const prevLastItem = prevSiblingList.lastChild!;

        const prevLastChildren: any[] = [];
        for (let i = 0; i < prevLastItem.childCount; i++) prevLastChildren.push(prevLastItem.child(i));

        const lastChild = prevLastChildren.length > 0 ? prevLastChildren[prevLastChildren.length - 1] : null;
        if (lastChild && lastChild.type === list.type) {
          const merged: any[] = [];
          for (let j = 0; j < lastChild.childCount; j++) merged.push(lastChild.child(j));
          for (const si of sunkItems) merged.push(si);
          prevLastChildren[prevLastChildren.length - 1] = list.type.create(lastChild.attrs, Fragment.from(merged));
        } else {
          prevLastChildren.push(list.type.create(null, Fragment.from(sunkItems)));
        }

        const newPrevLastItem = prevLastItem.type.create(prevLastItem.attrs, Fragment.from(prevLastChildren));
        const prevListItems: any[] = [];
        for (let i = 0; i < prevSiblingList.childCount - 1; i++) prevListItems.push(prevSiblingList.child(i));
        prevListItems.push(newPrevLastItem);
        const newPrevList = prevSiblingList.type.create(prevSiblingList.attrs, Fragment.from(prevListItems));

        // Remove sunk items from current list
        const lastIdx = group.indices[group.indices.length - 1];
        let firstSunkStart = mappedListPos + 1;
        for (let i = 0; i < group.indices[0]; i++) firstSunkStart += list.child(i).nodeSize;
        let lastSunkEnd = mappedListPos + 1;
        for (let i = 0; i <= lastIdx; i++) lastSunkEnd += list.child(i).nodeSize;

        const remainingBefore: any[] = [];
        for (let i = 0; i < group.indices[0]; i++) remainingBefore.push(list.child(i));
        const remainingAfter: any[] = [];
        for (let i = lastIdx + 1; i < list.childCount; i++) remainingAfter.push(list.child(i));
        const remaining = [...remainingBefore, ...remainingAfter];

        if (remaining.length === 0) {
          tr = tr.replaceWith(prevListStart, mappedListPos + list.nodeSize, newPrevList);
        } else {
          const newList = list.type.create(list.attrs, Fragment.from(remaining));
          tr = tr.replaceWith(prevListStart, mappedListPos + list.nodeSize, Fragment.from([newPrevList, newList]));
        }
      }
    }
  }

  const newFrom = posFromTextOffset(tr.doc, fromTextOff);
  const newTo = posFromTextOffset(tr.doc, toTextOff);
  try {
    tr.setSelection(TextSelection.create(tr.doc, newFrom, newTo));
  } catch (e) { /* fallback */ }

  dispatch(tr.scrollIntoView());
  return true;
};

/** Range-aware lift: outdent all selected list items, or none if any can't be outdented.
 *  Only activates for multi-item selections (2+ top-level items).
 *  Sibling items in the same list are lifted as a batch. */
export const liftSelectedItems = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  if (state.selection.from === state.selection.to) return false;
  const items = collectListItemsInSelection(state);
  if (items.length === 0) return false;

  const schema = state.schema;

  // Filter to shallowest selected items
  const selectedPositions = new Set(items.map(i => i.pos));
  const topItems = items.filter(item => {
    const $pos = state.doc.resolve(item.pos);
    for (let d = $pos.depth - 1; d >= 0; d--) {
      if (isAnyListItem(schema, $pos.node(d).type)) {
        const ancestorPos = $pos.before(d);
        if (selectedPositions.has(ancestorPos)) return false;
      }
    }
    return true;
  });

  if (topItems.length < 2) return false;

  // Group by parent list
  const byList = new Map<number, { items: typeof topItems; listDepth: number }>();
  for (const item of topItems) {
    const $pos = state.doc.resolve(item.pos);
    let listDepth = -1;
    for (let d = $pos.depth; d >= 0; d--) {
      if (isListNode(schema, $pos.node(d).type)) { listDepth = d; break; }
    }
    if (listDepth < 0) return false;
    const listPos = $pos.start(listDepth) - 1;
    if (!byList.has(listPos)) byList.set(listPos, { items: [], listDepth });
    byList.get(listPos)!.items.push(item);
  }

  if (!dispatch) return true;

  const fromTextOff = docTextOffset(state.doc, state.selection.from);
  const toTextOff = docTextOffset(state.doc, state.selection.to);

  // Process lists bottom-to-top
  const sortedListPositions = [...byList.keys()].sort((a, b) => b - a);
  let tr = state.tr;

  for (const origListPos of sortedListPositions) {
    const { listDepth } = byList.get(origListPos)!;
    const mappedListPos = tr.mapping.map(origListPos);
    const list = tr.doc.nodeAt(mappedListPos);
    if (!list || !isListNode(schema, list.type)) continue;

    const $listPos = tr.doc.resolve(mappedListPos);

    // Get selected indices
    const topItemMapped = new Set(topItems.map(i => tr.mapping.map(i.pos)));
    const selectedIndices = new Set<number>();
    let offset = mappedListPos + 1;
    for (let i = 0; i < list.childCount; i++) {
      if (topItemMapped.has(offset)) selectedIndices.add(i);
      offset += list.child(i).nodeSize;
    }

    const isNested = $listPos.depth >= 1 && isAnyListItem(schema, $listPos.node($listPos.depth).type);

    if (isNested) {
      // Nested list inside another list item — lift selected items to become siblings of outer item
      const outerItem = $listPos.node($listPos.depth);
      const outerItemType = outerItem.type;
      const outerListDepth = $listPos.depth - 1;
      const outerList = $listPos.node(outerListDepth);
      const outerListPos = $listPos.start(outerListDepth) - 1;
      const outerItemIndex = $listPos.index(outerListDepth);
      const innerListIndex = $listPos.index($listPos.depth); // index of inner list in outer item

      // Collect selected item nodes
      const liftedItems: any[] = [];
      const remainingItems: any[] = [];
      for (let i = 0; i < list.childCount; i++) {
        const child = list.child(i);
        if (selectedIndices.has(i)) {
          // Convert type to match outer list
          const converted = outerItemType.create(
            outerItemType === schema.nodes.checkListItem ? { checked: child.attrs.checked ?? false } : null,
            child.content
          );
          liftedItems.push(converted);
        } else {
          remainingItems.push(child);
        }
      }

      // Rebuild outer item: keep children before inner list, remaining inner list items, children after
      const outerItemBefore: any[] = [];
      const outerItemAfter: any[] = [];
      for (let i = 0; i < outerItem.childCount; i++) {
        if (i < innerListIndex) {
          outerItemBefore.push(outerItem.child(i));
        } else if (i === innerListIndex) {
          if (remainingItems.length > 0) {
            outerItemBefore.push(list.type.create(list.attrs, Fragment.from(remainingItems)));
          }
        } else {
          outerItemAfter.push(outerItem.child(i));
        }
      }

      // Outer item after nodes become children of the LAST lifted item
      if (outerItemAfter.length > 0 && liftedItems.length > 0) {
        const lastLifted = liftedItems[liftedItems.length - 1];
        const liftedChildren: any[] = [];
        for (let i = 0; i < lastLifted.childCount; i++) liftedChildren.push(lastLifted.child(i));
        for (const c of outerItemAfter) liftedChildren.push(c);
        liftedItems[liftedItems.length - 1] = outerItemType.create(lastLifted.attrs, Fragment.from(liftedChildren));
      }

      const modifiedOuterItem = outerItemType.create(outerItem.attrs, Fragment.from(outerItemBefore));

      const outerListItems: any[] = [];
      for (let i = 0; i <= outerItemIndex; i++) {
        outerListItems.push(i === outerItemIndex ? modifiedOuterItem : outerList.child(i));
      }
      for (const li of liftedItems) outerListItems.push(li);
      for (let i = outerItemIndex + 1; i < outerList.childCount; i++) outerListItems.push(outerList.child(i));

      const newOuterList = outerList.type.create(outerList.attrs, Fragment.from(outerListItems));
      tr = tr.replaceWith(outerListPos, outerListPos + outerList.nodeSize, newOuterList);
    } else {
      // Top-level list: unwrap selected items to paragraphs
      // Process from end to start by index
      const sortedIndices = [...selectedIndices].sort((a, b) => b - a);
      for (const idx of sortedIndices) {
        const itemNode = list.child(idx);
        const para = itemNode.firstChild!;

        // Recalculate position of this item in the current tr doc
        const curList = tr.doc.nodeAt(tr.mapping.map(origListPos));
        if (!curList) continue;
        const curListPos = tr.mapping.map(origListPos);
        let itemStart = curListPos + 1;
        for (let i = 0; i < idx; i++) itemStart += curList.child(i).nodeSize;
        const curItemNode = curList.child(idx);

        const nodes: any[] = [];
        if (idx > 0) {
          const before: any[] = [];
          for (let i = 0; i < idx; i++) before.push(curList.child(i));
          nodes.push(curList.type.create(curList.attrs, Fragment.from(before)));
        }
        nodes.push(para);
        for (let i = 1; i < curItemNode.childCount; i++) nodes.push(curItemNode.child(i));
        if (idx < curList.childCount - 1) {
          const after: any[] = [];
          for (let i = idx + 1; i < curList.childCount; i++) after.push(curList.child(i));
          nodes.push(curList.type.create(curList.attrs, Fragment.from(after)));
        }

        tr = tr.replaceWith(curListPos, curListPos + curList.nodeSize, nodes);
      }
    }
  }

  if (!tr.docChanged) return false;

  const newFrom = posFromTextOffset(tr.doc, fromTextOff);
  const newTo = posFromTextOffset(tr.doc, toTextOff);
  try {
    tr.setSelection(TextSelection.create(tr.doc, newFrom, newTo));
  } catch (e) { /* fallback */ }

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
  const itemType = $cursor.node(depth - 1).type;
  if (!isAnyListItem(state.schema, itemType)) return false;
  if ($cursor.index(depth - 2) === 0) return false; // first items handled by listBackspace

  if ($cursor.parent.content.size === 0) {
    return liftCurrentItem(state, dispatch);
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
  const itemType = $cursor.node(depth - 1).type;
  if (!isAnyListItem(state.schema, itemType)) return false;
  if ($cursor.index(depth - 2) !== 0) return false; // non-first items handled by joinListItems
  return liftCurrentItem(state, dispatch);
};

// ---------------------------------------------------------------------------
// Delete commands
// ---------------------------------------------------------------------------

// Delete on empty paragraph: if next sibling is a list, just remove the empty paragraph
const deleteEmptyParaBeforeList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor) return false;
  if ($cursor.parent.content.size !== 0) return false;
  if ($cursor.parent.type !== state.schema.nodes.paragraph) return false;

  const depth = $cursor.depth;
  const container = $cursor.node(depth - 1);
  // Don't handle if inside a list item — that's handled by liftEmptyListItem/etc.
  if (isAnyListItem(state.schema, container.type)) return false;

  const indexInParent = $cursor.index(depth - 1);
  if (indexInParent >= container.childCount - 1) return false;
  const nextSibling = container.child(indexInParent + 1);
  if (!isListNode(state.schema, nextSibling.type)) return false;

  if (!dispatch) return true;

  const paraStart = $cursor.before(depth);
  const paraEnd = $cursor.after(depth);
  const tr = state.tr.delete(paraStart, paraEnd);
  tr.setSelection(TextSelection.near(tr.doc.resolve(paraStart), 1));
  dispatch(tr.scrollIntoView());
  return true;
};

// Delete on empty list item that's the only item in its list:
// Remove the list entirely, and join adjacent same-type lists if possible
const deleteEmptyListItem = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor) return false;
  if ($cursor.parent.content.size !== 0) return false;
  if ($cursor.parent.type !== state.schema.nodes.paragraph) return false;

  const depth = $cursor.depth;
  const schema = state.schema;
  const itemType = findItemType(state);
  if (!itemType) return false;

  let liDepth = -1;
  for (let d = $cursor.depth; d >= 0; d--) {
    if ($cursor.node(d).type === itemType) { liDepth = d; break; }
  }
  if (liDepth < 1) return false;

  const listItem = $cursor.node(liDepth);
  if (listItem.childCount !== 1) return false; // only handle single-paragraph (empty) items

  const list = $cursor.node(liDepth - 1);
  if (list.childCount !== 1) return false; // only handle single-item lists

  const listDepth = liDepth - 1;
  if (listDepth < 1) return false;
  const container = $cursor.node(listDepth - 1);
  const listIndex = $cursor.index(listDepth - 1);

  if (!dispatch) return true;

  const listPos = $cursor.start(listDepth) - 1;

  // Check if prev and next siblings are lists of the same type — join them
  const prevSibling = listIndex > 0 ? container.child(listIndex - 1) : null;
  const nextSibling = listIndex < container.childCount - 1 ? container.child(listIndex + 1) : null;

  if (prevSibling && nextSibling &&
      isListNode(schema, prevSibling.type) && prevSibling.type === nextSibling.type) {
    // Join: remove current list, merge prev and next into one
    const prevItems: any[] = [];
    for (let i = 0; i < prevSibling.childCount; i++) prevItems.push(prevSibling.child(i));
    const nextItems: any[] = [];
    for (let i = 0; i < nextSibling.childCount; i++) nextItems.push(nextSibling.child(i));
    const merged = prevSibling.type.create(prevSibling.attrs, Fragment.from([...prevItems, ...nextItems]));

    const prevListPos = listPos - prevSibling.nodeSize;
    const nextListEnd = listPos + list.nodeSize + nextSibling.nodeSize;
    const tr = state.tr.replaceWith(prevListPos, nextListEnd, merged);
    // Place cursor at the join point (end of last prev item)
    let pos = prevListPos + 1;
    for (let i = 0; i < prevItems.length; i++) pos += prevItems[i].nodeSize;
    const sel = TextSelection.near(tr.doc.resolve(pos), -1);
    tr.setSelection(sel);
    dispatch(tr.scrollIntoView());
    return true;
  }

  // No join — just remove the single-item list
  const tr = state.tr.delete(listPos, listPos + list.nodeSize);
  const sel = TextSelection.near(tr.doc.resolve(listPos), listIndex > 0 ? -1 : 1);
  tr.setSelection(sel);
  dispatch(tr.scrollIntoView());
  return true;
};

// Delete key: merge next item's text into current, adopting its children.
//   Case A: - A| + - B  →  - AB       Case B: A| + - B  →  AB
//             - C             - C               - C          - C
const deleteIntoList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor) return false;
  if ($cursor.parentOffset !== $cursor.parent.content.size) return false;

  const depth = $cursor.depth;
  const schema = state.schema;
  const curParaContentEnd = $cursor.pos;

  // Case A: inside a list item, next sibling item exists
  if (depth >= 2 && isAnyListItem(schema, $cursor.node(depth - 1).type)) {
    const listItemType = $cursor.node(depth - 1).type;
    const curItem = $cursor.node(depth - 1);
    const paraIndexInItem = $cursor.index(depth - 1);

    // Case A0: paragraph is followed by a sublist within the same list item
    // Merge the first item's paragraph of that sublist into the current paragraph
    if (paraIndexInItem < curItem.childCount - 1) {
      const nextChild = curItem.child(paraIndexInItem + 1);
      if (isListNode(schema, nextChild.type)) {
        const firstItem = nextChild.firstChild;
        if (firstItem && isAnyListItem(schema, firstItem.type) &&
            firstItem.firstChild && firstItem.firstChild.type === schema.nodes.paragraph) {
          if (!dispatch) return true;

          const curPara = $cursor.parent;
          const nextPara = firstItem.firstChild!;

          const mergedPara = schema.nodes.paragraph.create(
            curPara.attrs,
            curPara.content.append(nextPara.content)
          );

          // Promote first item's children (sublists) + remaining items in the sublist
          const newChildren: any[] = [mergedPara];
          // Children before the sublist (shouldn't be any since para is at paraIndexInItem, but safe)
          for (let i = 1; i <= paraIndexInItem; i++) newChildren.push(curItem.child(i));
          // First item's own sublist children
          for (let i = 1; i < firstItem.childCount; i++) newChildren.push(firstItem.child(i));
          // Remaining items in the sublist (as a new list, if any)
          if (nextChild.childCount > 1) {
            const remaining: any[] = [];
            for (let i = 1; i < nextChild.childCount; i++) remaining.push(nextChild.child(i));
            newChildren.push(nextChild.type.create(nextChild.attrs, Fragment.from(remaining)));
          }
          // Children after the sublist in the list item
          for (let i = paraIndexInItem + 2; i < curItem.childCount; i++) newChildren.push(curItem.child(i));

          const newItem = listItemType.create(curItem.attrs, Fragment.from(newChildren));
          const curItemStart = $cursor.before(depth - 1);
          const tr = state.tr.replaceWith(curItemStart, curItemStart + curItem.nodeSize, newItem);
          tr.setSelection(TextSelection.create(tr.doc, curParaContentEnd));
          dispatch(tr.scrollIntoView());
          return true;
        }
      }
    }

    // Only when paragraph is last child
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

  // Case A2: cursor at end of last item in a list, next sibling of the list
  // is another list — merge the first item's text of that list into current item.
  //   [] A|       →   [] AB
  //   1. B              1. C
  //   2. C
  if (depth >= 2 && isAnyListItem(schema, $cursor.node(depth - 1).type)) {
    const curItem = $cursor.node(depth - 1);
    const paraIndexInItem = $cursor.index(depth - 1);

    // Cursor must be at end of the item's paragraph (possibly not last child if sublists exist)
    if (paraIndexInItem === 0 && curItem.firstChild!.type === schema.nodes.paragraph) {
      const list = $cursor.node(depth - 2);
      const listDepth = depth - 2;
      const listItemIndex = $cursor.index(depth - 2);

      // Only if this is the last item in its list
      if (listItemIndex === list.childCount - 1) {
        // Check what comes after this list in its parent container
        const listContainer = $cursor.node(listDepth - 1);
        const listIndexInContainer = $cursor.index(listDepth - 1);

        if (listIndexInContainer < listContainer.childCount - 1) {
          const nextAfterList = listContainer.child(listIndexInContainer + 1);

          if (isListNode(schema, nextAfterList.type)) {
            // Next sibling is a list — merge its first item's paragraph into ours
            const nextFirstItem = nextAfterList.firstChild;
            if (nextFirstItem && isAnyListItem(schema, nextFirstItem.type) &&
                nextFirstItem.firstChild && nextFirstItem.firstChild.type === schema.nodes.paragraph) {
              if (!dispatch) return true;

              const nextPara = nextFirstItem.firstChild!;
              const nextListPos = $cursor.end(listDepth) + 1; // position after </list>

              // Remove first item from next list (or remove entire list if only 1 item)
              let tr = state.tr;
              const promotedItems: any[] = [];
              for (let i = 1; i < nextFirstItem.childCount; i++) {
                const child = nextFirstItem.child(i);
                if (isListNode(schema, child.type)) {
                  for (let j = 0; j < child.childCount; j++) promotedItems.push(child.child(j));
                }
              }

              if (nextAfterList.childCount === 1 && promotedItems.length === 0) {
                tr = tr.delete(nextListPos, nextListPos + nextAfterList.nodeSize);
              } else {
                const newListItems: any[] = [...promotedItems];
                for (let i = 1; i < nextAfterList.childCount; i++) newListItems.push(nextAfterList.child(i));
                if (newListItems.length > 0) {
                  const newList = nextAfterList.type.create(nextAfterList.attrs, Fragment.from(newListItems));
                  tr = tr.replaceWith(nextListPos, nextListPos + nextAfterList.nodeSize, newList);
                } else {
                  tr = tr.delete(nextListPos, nextListPos + nextAfterList.nodeSize);
                }
              }

              tr = tr.insert(curParaContentEnd, nextPara.content);
              tr.setSelection(TextSelection.create(tr.doc, curParaContentEnd));
              dispatch(tr.scrollIntoView());
              return true;
            }
          } else if (nextAfterList.type === schema.nodes.paragraph) {
            // Next sibling is a paragraph — merge its text into current item
            if (!dispatch) return true;

            const nextParaPos = $cursor.end(listDepth) + 1;
            let tr = state.tr;
            tr = tr.delete(nextParaPos, nextParaPos + nextAfterList.nodeSize);
            tr = tr.insert(curParaContentEnd, nextAfterList.content);
            tr.setSelection(TextSelection.create(tr.doc, curParaContentEnd));
            dispatch(tr.scrollIntoView());
            return true;
          }
        }
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
  if (!firstItem || !isAnyListItem(schema, firstItem.type)) return false;
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
  if (depth >= 1 && isAnyListItem(schema, $cursor.node(depth - 1).type)) return false;

  const indexInParent = $cursor.index(depth - 1);
  if (indexInParent === 0) return false;

  const parent = $cursor.node(depth - 1);
  const prevSibling = parent.child(indexInParent - 1);
  if (!isListNode(schema, prevSibling.type)) return false;

  const lastItem = prevSibling.lastChild;
  if (!lastItem || !isAnyListItem(schema, lastItem.type)) return false;

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
    if (!nestedItem || !isAnyListItem(schema, nestedItem.type)) break;
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

// Convert only the current item to a different list type, splitting the parent list as needed.
// E.g. bulletList[A, B, C] with cursor on B → bulletList[A] + checkList[B] + bulletList[C]
function convertCurrentItem(state: EditorState, fromList: NodeType, toList: NodeType, toItemType: NodeType, dispatch?: (tr: Transaction) => void): boolean {
  const { $from } = state.selection;
  const schema = state.schema;

  // Find the list and item depths
  let listDepth = -1;
  for (let d = $from.depth; d >= 0; d--) {
    if ($from.node(d).type === fromList) { listDepth = d; break; }
  }
  if (listDepth < 0) return false;

  const list = $from.node(listDepth);
  const listPos = $from.start(listDepth) - 1; // start() is content start, -1 for node open token
  const itemIndex = $from.index(listDepth);
  const cursorPos = $from.pos;

  if (!dispatch) return true;

  // Build the replacement fragments: [before items as original list] + [converted item as new list] + [after items as original list]
  const item = list.child(itemIndex);
  const attrs = toItemType === schema.nodes.checkListItem ? { checked: false } : null;
  const newItem = toItemType.create(attrs, item.content, item.marks);
  const newSingleList = toList.create(null, Fragment.from([newItem]));

  const nodes: any[] = [];

  // Items before the current one — keep as original list type
  if (itemIndex > 0) {
    const beforeItems: any[] = [];
    for (let i = 0; i < itemIndex; i++) beforeItems.push(list.child(i));
    nodes.push(fromList.create(list.attrs, Fragment.from(beforeItems)));
  }

  nodes.push(newSingleList);

  // Items after the current one — keep as original list type
  if (itemIndex < list.childCount - 1) {
    const afterItems: any[] = [];
    for (let i = itemIndex + 1; i < list.childCount; i++) afterItems.push(list.child(i));
    nodes.push(fromList.create(list.attrs, Fragment.from(afterItems)));
  }

  // Compute cursor offset relative to the item start
  let itemStart = listPos + 1; // skip list open token
  for (let i = 0; i < itemIndex; i++) itemStart += list.child(i).nodeSize;
  const cursorOffsetInItem = cursorPos - itemStart;

  const tr = state.tr.replaceWith(listPos, listPos + list.nodeSize, nodes);

  // Compute new item position: listPos + size of before-list (if any) + 1 (new list open token)
  let newItemStart = listPos;
  if (itemIndex > 0) {
    const beforeItems: any[] = [];
    for (let i = 0; i < itemIndex; i++) beforeItems.push(list.child(i));
    newItemStart += fromList.create(list.attrs, Fragment.from(beforeItems)).nodeSize;
  }
  newItemStart += 1; // enter the new single list

  const newCursorPos = newItemStart + cursorOffsetInItem;
  tr.setSelection(TextSelection.create(tr.doc, newCursorPos));
  dispatch(tr);
  return true;
}

// ---------------------------------------------------------------------------
// Range-aware list toggle: converts all touched paragraphs & list items
// ---------------------------------------------------------------------------

type ItemInfo = {
  itemPos: number;
  itemNode: any;
  listPos: number;
  listNode: any;
  itemIndex: number;
};

function collectSelectionTargets(state: EditorState) {
  const { from, to } = state.selection;
  const schema = state.schema;
  const seenItems = new Set<number>();
  const items: ItemInfo[] = [];
  const plainParas: number[] = [];

  state.doc.nodesBetween(from, to, (node, pos) => {
    if (node.type === schema.nodes.paragraph) {
      const $pos = state.doc.resolve(pos);
      let found = false;
      for (let d = $pos.depth; d >= 0; d--) {
        if (isAnyListItem(schema, $pos.node(d).type)) {
          const itemPos = $pos.start(d) - 1;
          if (!seenItems.has(itemPos)) {
            seenItems.add(itemPos);
            items.push({
              itemPos,
              itemNode: $pos.node(d),
              listPos: $pos.start(d - 1) - 1,
              listNode: $pos.node(d - 1),
              itemIndex: $pos.index(d - 1),
            });
          }
          found = true;
          break;
        }
      }
      if (!found) plainParas.push(pos);
      return false;
    }
    return true;
  });

  return { items, plainParas };
}

function setListTypeForSelection(
  targetList: NodeType,
  targetItem: NodeType,
  state: EditorState,
  dispatch?: (tr: Transaction) => void
): boolean {
  const schema = state.schema;
  const { items, plainParas } = collectSelectionTargets(state);

  if (items.length === 0 && plainParas.length === 0) return false;

  // Toggle off: if all items are already target type and no plain paragraphs
  if (plainParas.length === 0 && items.every(i => i.listNode.type === targetList)) {
    return pmLiftListItem(targetItem)(state, dispatch);
  }

  if (!dispatch) return true;

  let tr = state.tr;

  // Group items by parent list position
  const byList = new Map<number, ItemInfo[]>();
  for (const item of items) {
    if (item.listNode.type === targetList) continue; // already correct
    const arr = byList.get(item.listPos) || [];
    arr.push(item);
    byList.set(item.listPos, arr);
  }

  // Process lists from bottom to top (by position descending)
  const sortedListPositions = [...byList.keys()].sort((a, b) => b - a);

  for (const listPos of sortedListPositions) {
    const listItems = byList.get(listPos)!;
    const mappedListPos = tr.mapping.map(listPos);
    const list = tr.doc.nodeAt(mappedListPos);
    if (!list || !isListNode(schema, list.type)) continue;

    const selectedIndices = new Set(listItems.map(i => i.itemIndex));

    // Check if all items in the list are selected
    let allSelected = true;
    for (let i = 0; i < list.childCount; i++) {
      if (!selectedIndices.has(i)) { allSelected = false; break; }
    }

    if (allSelected) {
      // Convert entire list in place
      const newItems: any[] = [];
      for (let i = 0; i < list.childCount; i++) {
        const child = list.child(i);
        const attrs = targetItem === schema.nodes.checkListItem ? { checked: false } : null;
        newItems.push(targetItem.create(attrs, child.content, child.marks));
      }
      const newList = targetList.create(null, Fragment.from(newItems));
      tr = tr.replaceWith(mappedListPos, mappedListPos + list.nodeSize, newList);
    } else {
      // Split: selected items become target type, others stay
      const nodes: any[] = [];
      let keepBatch: any[] = [];

      for (let i = 0; i < list.childCount; i++) {
        const child = list.child(i);
        if (selectedIndices.has(i)) {
          if (keepBatch.length > 0) {
            nodes.push(list.type.create(list.attrs, Fragment.from(keepBatch)));
            keepBatch = [];
          }
          const attrs = targetItem === schema.nodes.checkListItem ? { checked: false } : null;
          const newItem = targetItem.create(attrs, child.content, child.marks);
          // Merge with previous target-type list if adjacent
          const last = nodes.length > 0 ? nodes[nodes.length - 1] : null;
          if (last && last.type === targetList) {
            const prevItems: any[] = [];
            for (let j = 0; j < last.childCount; j++) prevItems.push(last.child(j));
            prevItems.push(newItem);
            nodes[nodes.length - 1] = targetList.create(null, Fragment.from(prevItems));
          } else {
            nodes.push(targetList.create(null, Fragment.from([newItem])));
          }
        } else {
          keepBatch.push(child);
        }
      }
      if (keepBatch.length > 0) {
        nodes.push(list.type.create(list.attrs, Fragment.from(keepBatch)));
      }

      tr = tr.replaceWith(mappedListPos, mappedListPos + list.nodeSize, nodes);
    }
  }

  // Wrap plain paragraphs in target list (bottom to top)
  const sortedParas = [...plainParas].sort((a, b) => b - a);
  for (const paraPos of sortedParas) {
    const mappedPos = tr.mapping.map(paraPos);
    const para = tr.doc.nodeAt(mappedPos);
    if (!para || para.type !== schema.nodes.paragraph) continue;
    const attrs = targetItem === schema.nodes.checkListItem ? { checked: false } : null;
    const item = targetItem.create(attrs, Fragment.from([para]));
    const wrappedList = targetList.create(null, Fragment.from([item]));
    tr = tr.replaceWith(mappedPos, mappedPos + para.nodeSize, wrappedList);
  }

  // Join adjacent same-type lists within the affected range
  // Scan from end to start so positions stay valid
  const joinPositions: number[] = [];
  tr.doc.descendants((node, pos) => {
    if (!node.isBlock || node.isTextblock) return true;
    for (let i = 0; i < node.childCount - 1; i++) {
      const a = node.child(i);
      const b = node.child(i + 1);
      if (isListNode(schema, a.type) && a.type === b.type) {
        let offset = 1;
        for (let j = 0; j <= i; j++) offset += node.child(j).nodeSize;
        joinPositions.push(pos + offset);
      }
    }
    return true;
  });
  joinPositions.sort((a, b) => b - a);
  for (const joinPos of joinPositions) {
    const $j = tr.doc.resolve(joinPos);
    if ($j.nodeBefore && $j.nodeAfter && $j.nodeBefore.type === $j.nodeAfter.type) {
      tr = tr.join(joinPos);
    }
  }

  // Preserve selection by text offset (structure changes but text doesn't)
  const fromTextOff = docTextOffset(state.doc, state.selection.from);
  const toTextOff = docTextOffset(state.doc, state.selection.to);
  const newFrom = posFromTextOffset(tr.doc, fromTextOff);
  const newTo = posFromTextOffset(tr.doc, toTextOff);
  try {
    tr.setSelection(TextSelection.create(tr.doc, newFrom, newTo));
  } catch (e) {
    // fallback: don't set selection, let ProseMirror pick
  }

  dispatch(tr.scrollIntoView());
  return true;
}

export const toggleBulletList = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const schema = state.schema;
  return setListTypeForSelection(schema.nodes.bulletList, schema.nodes.listItem, state, dispatch);
};

export const toggleOrderedList = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const schema = state.schema;
  return setListTypeForSelection(schema.nodes.orderedList, schema.nodes.listItem, state, dispatch);
};

export const toggleCheckList = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const schema = state.schema;
  return setListTypeForSelection(schema.nodes.checkList, schema.nodes.checkListItem, state, dispatch);
};

// ---------------------------------------------------------------------------
// Split checklist item — creates new unchecked item
// ---------------------------------------------------------------------------

// Enter on an empty list item: lift using our custom handler (not pmLiftListItem)
const liftEmptyListItem = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const { $from } = state.selection;
  const schema = state.schema;
  const itemType = findItemType(state);
  if (!itemType) return false;

  // Must be an empty paragraph inside a list item
  if ($from.parent.content.size !== 0) return false;
  if ($from.parent.type !== schema.nodes.paragraph) return false;

  let liDepth = -1;
  for (let d = $from.depth; d >= 0; d--) {
    if ($from.node(d).type === itemType) { liDepth = d; break; }
  }
  if (liDepth < 0) return false;

  const listItem = $from.node(liDepth);
  // Only handle single-paragraph items (no children)
  if (listItem.childCount !== 1) return false;

  return liftCurrentItem(state, dispatch);
};

const splitCheckListItem = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  return pmSplitListItem(state.schema.nodes.checkListItem, { checked: false })(state, dispatch);
};

// ---------------------------------------------------------------------------
// Input rules
// ---------------------------------------------------------------------------

// Convert-in-place input rule: when typing a list shorthand at the start of a
// list item's paragraph, convert the surrounding list type instead of nesting.
function listConvertRule(pattern: RegExp, targetList: NodeType, targetItem: NodeType, schema: Schema): InputRule {
  return new InputRule(pattern, (state, match, start, end) => {
    const $start = state.doc.resolve(start);
    // Check if we're at the very start of a paragraph inside a list item
    if ($start.parentOffset !== 0) return null;
    const itemDepth = findItemDepth($start, schema);
    if (itemDepth === null) return null;

    const currentListType = $start.node(itemDepth - 1).type;
    if (currentListType === targetList) return null; // already the right type

    const fromList = currentListType;
    const list = $start.node(itemDepth - 1);
    const listPos = $start.start(itemDepth - 1) - 1;
    const itemIndex = $start.index(itemDepth - 1);

    let tr = state.tr.delete(start, end); // delete the typed shorthand

    // Re-read list from post-delete doc
    const postList = tr.doc.nodeAt(listPos)!;
    const item = postList.child(itemIndex);
    const attrs = targetItem === schema.nodes.checkListItem ? { checked: false } : null;
    const newItem = targetItem.create(attrs, item.content, item.marks);
    const newSingleList = targetList.create(null, Fragment.from([newItem]));

    const nodes: any[] = [];

    if (itemIndex > 0) {
      const beforeItems: any[] = [];
      for (let i = 0; i < itemIndex; i++) beforeItems.push(postList.child(i));
      nodes.push(fromList.create(postList.attrs, Fragment.from(beforeItems)));
    }

    nodes.push(newSingleList);

    if (itemIndex < postList.childCount - 1) {
      const afterItems: any[] = [];
      for (let i = itemIndex + 1; i < postList.childCount; i++) afterItems.push(postList.child(i));
      nodes.push(fromList.create(postList.attrs, Fragment.from(afterItems)));
    }

    tr = tr.replaceWith(listPos, listPos + postList.nodeSize, nodes);

    // Place cursor at start of the converted item's paragraph
    let newItemStart = listPos;
    if (itemIndex > 0) {
      const beforeItems: any[] = [];
      for (let i = 0; i < itemIndex; i++) beforeItems.push(postList.child(i));
      newItemStart += fromList.create(postList.attrs, Fragment.from(beforeItems)).nodeSize;
    }
    newItemStart += 1; // enter new list
    newItemStart += 1; // enter item
    newItemStart += 1; // enter paragraph
    tr.setSelection(TextSelection.create(tr.doc, newItemStart));
    return tr;
  });
}

function findItemDepth($pos: any, schema: Schema): number | null {
  for (let d = $pos.depth; d >= 0; d--) {
    if (isAnyListItem(schema, $pos.node(d).type)) return d;
  }
  return null;
}

function makeListInputRules(schema: Schema) {
  return inputRules({
    rules: [
      // Convert rules (must come before wrapping rules — they handle the "already in a list" case)
      listConvertRule(/^[-*]\s$/, schema.nodes.bulletList, schema.nodes.listItem, schema),
      listConvertRule(/^(\d{1,2})[.)]\s$/, schema.nodes.orderedList, schema.nodes.listItem, schema),
      listConvertRule(/^\[( ?)\]\s$/, schema.nodes.checkList, schema.nodes.checkListItem, schema),

      // Wrapping rules (handle the "not in a list" case)
      wrappingInputRule(/^[-*]\s$/, schema.nodes.bulletList),
      wrappingInputRule(
        /^(\d{1,2})\.\s$/,
        schema.nodes.orderedList,
        (match: RegExpMatchArray) => ({ order: +match[1] }),
        (match: RegExpMatchArray, node: any) => node.childCount + node.attrs.order === +match[1]
      ),
      wrappingInputRule(
        /^(\d{1,2})\)\s$/,
        schema.nodes.orderedList,
        (match: RegExpMatchArray) => ({ order: +match[1] }),
        (match: RegExpMatchArray, node: any) => node.childCount + node.attrs.order === +match[1]
      ),
      wrappingInputRule(
        /^\[( ?)\]\s$/,
        schema.nodes.checkList,
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
  const checkListItemType = schema.nodes.checkListItem;

  const backspaceChain = [
    ...extraBackspace,
    joinListItems,
    listBackspace,
    joinAfterList,
    joinBackward,
    selectNodeBackward,
  ];

  // Plugin that auto-joins adjacent same-type lists after any document change
  const joinAdjacentLists = new Plugin({
    appendTransaction(transactions, oldState, newState) {
      if (!transactions.some(tr => tr.docChanged)) return null;

      const joinPositions: number[] = [];
      newState.doc.descendants((node, pos) => {
        if (!node.isBlock || node.isTextblock) return true;
        for (let i = 0; i < node.childCount - 1; i++) {
          const a = node.child(i);
          const b = node.child(i + 1);
          if (isListNode(schema, a.type) && a.type === b.type) {
            let offset = 1;
            for (let j = 0; j <= i; j++) offset += node.child(j).nodeSize;
            joinPositions.push(pos + offset);
          }
        }
        return true;
      });

      if (joinPositions.length === 0) return null;

      // Join from bottom to top
      joinPositions.sort((a, b) => b - a);
      let tr = newState.tr;
      for (const joinPos of joinPositions) {
        const $j = tr.doc.resolve(joinPos);
        if ($j.nodeBefore && $j.nodeAfter && $j.nodeBefore.type === $j.nodeAfter.type) {
          tr = tr.join(joinPos);
        }
      }
      return tr.docChanged ? tr : null;
    },
  });

  return [
    makeListInputRules(schema),
    keymap({
      Tab: chainCommands(sinkSelectedItems, sinkIntoSiblingList, sinkCurrentItem),
      "Shift-Tab": chainCommands(liftSelectedItems, liftCurrentItem),
      Enter: chainCommands(liftEmptyListItem, splitCheckListItem, pmSplitListItem(listItemType)),
      Backspace: chainCommands(...backspaceChain),
      "Mod-Backspace": chainCommands(joinListItems, listBackspace, joinAfterList),
      Delete: chainCommands(deleteEmptyParaBeforeList, deleteEmptyListItem, deleteIntoList, joinForward, selectNodeForward),
    }),
    joinAdjacentLists,
  ];
}
