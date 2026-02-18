import { EditorState, Transaction, TextSelection } from "prosemirror-state";
import { toggleMark } from "prosemirror-commands";
import { Schema, NodeType } from "prosemirror-model";
import { textSchema } from "./textSchema";
import { Slice, Node, Mark, Fragment } from "prosemirror-model";
import { v4 as uuidv4 } from "uuid";
import * as phistory from "prosemirror-history";
import { wrapInList, splitListItem, liftListItem } from "prosemirror-schema-list";
import { inputRules, wrappingInputRule } from "prosemirror-inputrules";


export const undo = phistory.undo;
export const redo = phistory.redo;

export const toggleBold = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const markType = state.schema.marks.strong;
  return toggleMark(markType)(state, dispatch);
};

export const toggleItalic = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const markType = state.schema.marks.em;
  return toggleMark(markType)(state, dispatch);
};

export const toggleUnderline = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const markType = state.schema.marks.underline;
  return toggleMark(markType)(state, dispatch);
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
        headerDiff = newHeader.nodeSize - sectionHeaderSize + 1;
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

const nodeIsChunk = (node: Node) => {
  if (node.type.name === "section") return true;
  if (node.type.name === "bibleText") return true;
  if (node.type.name != "chunk") return false;
};


export const increaseLevel = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  let from = state.selection.from;
  let to = state.selection.to;
  let toTransform = [];
  // Why is this stupid?
  state.doc.nodesBetween(from, to, (node, pos) => {
    const result = nodeIsChunk(node);
    if (result === true) return true;
    if (result === false) return false;
    toTransform.push({ pos, node });
    return false;
  });
  if (toTransform.length == 0) return false;
  let type = textSchema.nodes.chunk;
  if (dispatch) {
    dispatch(
      toTransform.reduce((pre, { pos, node }) => {
        return pre.setNodeMarkup(pos, type, { level: node.attrs.level + 1 }, null);
      }, state.tr)
    );
  }
  return true;
};

export const decreaseLevel = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  let type = textSchema.nodes.chunk;
  let from = state.selection.from;
  let to = state.selection.to;
  let toTransform = [];
  // Why is this stupid?
  state.doc.nodesBetween(from, to, (node, pos) => {
    const result = nodeIsChunk(node);
    if (result === true) return true;
    if (result === false) return false;
    toTransform.push({ pos, node });
    return false;
  });
  if (toTransform.length == 0) return false;
  let nextState = toTransform.reduce((pre, { pos, node }) => {
    let level = node.attrs.level;
    let nextLevel = level != 0 ? level - 1 : level;
    return pre.setNodeMarkup(pos, type, { level: nextLevel }, null);
  }, state.tr);
  if (dispatch) dispatch(nextState);
  return true;
};


function newSectionNode(): Node {
  const header = textSchema.text("Untitled");
  const children = [textSchema.nodes.sectionHeader.create(null, header)];
  // crSection.bibleSections.map(newBibleText).forEach( (bs) => children.push(bs));
  const questions = textSchema.nodes.questions.createChecked();
  const studyBlock = textSchema.nodes.studyBlocks.createChecked(null, questions);
  children.push(studyBlock);
  const section = textSchema.nodes.section.createChecked(null, children);
  return section;
}


export const addSection = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  if (dispatch) {
    const node = newSectionNode();
    // should be the end of the document.
    const pos = state.doc.nodeSize - 2;

    const tr = state.tr.insert(pos, node);

    const selection = new TextSelection(
      tr.doc.resolve(pos),
      tr.doc.resolve(pos+10)
    );
    tr.setSelection(selection)
    tr.scrollIntoView();

    dispatch(tr);
    return true;
  }
};


export const moveSection = (
  originalIndex: number,
  newIndex: number,
) => (
  state: EditorState,
  dispatch?: (tr: Transaction) => void
) => {
  if (dispatch) {
    let originalPos: number = null;
    let originalNode: Node = null;
    let index = 0;
    state.doc.descendants((node: Node, pos: number) => {
      if (node.type.name === "section") {
        if (index == originalIndex) {
          originalPos = pos;
          originalNode = node;
        }
        index++;
        return false;
      }
      return false;
    });
    let tr = state.tr.deleteRange(originalPos, originalPos + originalNode.nodeSize);

    let newPos = null;
    index = 0;
    let lastNode = null;
    let lastPos = null;
    tr.doc.descendants((node: Node, pos: number) => {
      if (node.type.name === "section") {
        lastNode = node;
        lastPos = pos;
        if (index == newIndex) {
          newPos = pos;
        }
        index++;
        return false;
      }
      return false;
    });

    if (newPos === null) {
      newPos = lastPos + lastNode.nodeSize;
    }

    tr.insert(newPos, originalNode);

    dispatch(tr);
    return true;
  }
}

export const deleteSection = (
  sectionIndex : number
) => (
  state: EditorState,
  dispatch?: (tr: Transaction) => void
) => {

  if(dispatch) {
    let sectionPositions: number[] = [];
    state.doc.descendants((node, pos) => {
      if (node.type.name === "section") {
        sectionPositions.push(pos);
      }
    });

    if (sectionIndex >= sectionPositions.length) {
      console.error("Invalid section index");
      return;
    }

    const sectionPos = sectionPositions[sectionIndex];
    const sectionNode = state.doc.nodeAt(sectionPos);

    if (!sectionNode) {
      console.error("Couldn't find section node");
      return;
    }

    // Deleting the section
    const tr = state.tr.delete(sectionPos, sectionPos + sectionNode.nodeSize);
    dispatch(tr);
  }
}

// For non-first list items: if the current paragraph is empty, lift the item out of the list
// (removes the bullet, keeps the line as a plain paragraph). If the paragraph has content,
// merge it into the previous item's paragraph (standard join behavior).
export const joinListItems = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor || $cursor.parentOffset !== 0) return false;
  const depth = $cursor.depth;
  if (depth < 2) return false;
  if ($cursor.node(depth - 1).type !== state.schema.nodes.listItem) return false;
  // Only handle non-first items — first items are handled by listBackspace
  if ($cursor.index(depth - 2) === 0) return false;

  // Empty paragraph: lift item out of the list (remove bullet, keep the line).
  if ($cursor.parent.content.size === 0) {
    return liftListItem(state.schema.nodes.listItem)(state, dispatch);
  }

  // Non-empty paragraph: merge into the previous item's paragraph.
  // Only handle the simple case: prev listItem has exactly one child (a paragraph).
  const list = $cursor.node(depth - 2);
  const prevListItem = list.child($cursor.index(depth - 2) - 1);
  if (prevListItem.childCount !== 1) return false;
  if (prevListItem.firstChild!.type !== state.schema.nodes.paragraph) return false;

  if (dispatch) {
    // Delete the 4-token boundary: </prevPara> </prevListItem> <curListItem> <curPara>
    // This merges the two list items' paragraphs into one paragraph in prevListItem.
    const tr = state.tr.delete($cursor.pos - 4, $cursor.pos);
    dispatch(tr.scrollIntoView());
  }
  return true;
};

// For first list items: lift the item out of the list (convert to plain paragraph).
export const listBackspace = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor || $cursor.parentOffset !== 0) return false;
  const depth = $cursor.depth;
  if (depth < 2) return false;
  if ($cursor.node(depth - 1).type !== state.schema.nodes.listItem) return false;
  // Only fire for the first item — non-first items handled by joinListItems
  if ($cursor.index(depth - 2) !== 0) return false;
  return liftListItem(state.schema.nodes.listItem)(state, dispatch);
};

// For Delete key: merge the next list item's first paragraph into the current paragraph.
// Handles two cases:
//   A) cursor is at end of a listItem's paragraph, and there is a next sibling listItem
//   B) cursor is at end of a non-list paragraph, and the next sibling node is a list
export const deleteIntoList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor) return false;
  if ($cursor.parentOffset !== $cursor.parent.content.size) return false;

  const depth = $cursor.depth;
  const listTypes = [state.schema.nodes.bulletList, state.schema.nodes.orderedList];
  const curParaContentEnd = $cursor.pos;

  // Case A: cursor is at end of a listItem's last child (paragraph), and there is a
  // next sibling listItem in the same parent list. Merge next sibling's first paragraph.
  if (depth >= 2 && $cursor.node(depth - 1).type === state.schema.nodes.listItem) {
    const listItem = $cursor.node(depth - 1);
    const paraIndexInItem = $cursor.index(depth - 1);

    // Paragraph must be the last child of the listItem (nothing else to cross into)
    if (paraIndexInItem === listItem.childCount - 1) {
      const list = $cursor.node(depth - 2);
      const listItemIndex = $cursor.index(depth - 2);

      if (listItemIndex < list.childCount - 1) {
        const nextListItem = list.child(listItemIndex + 1);
        const nextFirstChild = nextListItem.firstChild;
        if (nextFirstChild && nextFirstChild.type === state.schema.nodes.paragraph) {
          const nextPara = nextFirstChild;
          // Position right after the current listItem's closing token — where nextListItem starts.
          const listItemAfter = $cursor.after(depth - 1);

          if (dispatch) {
            let tr = state.tr;
            // Delete the entire next listItem (including any nested content it may contain).
            tr = tr.delete(listItemAfter, listItemAfter + nextListItem.nodeSize);
            // Insert next paragraph's content into the current paragraph.
            tr = tr.insert(curParaContentEnd, nextPara.content);
            // Keep cursor at its original position (not at end of inserted content).
            tr = tr.setSelection(TextSelection.create(tr.doc, curParaContentEnd));
            dispatch(tr.scrollIntoView());
          }
          return true;
        }
      }
    }
  }

  // Case B: cursor at end of a paragraph whose next sibling is a list.
  // Merge the first list item's paragraph content into the current paragraph.
  const container = $cursor.node(depth - 1);
  const indexInParent = $cursor.index(depth - 1);
  if (indexInParent >= container.childCount - 1) return false;
  const nextSibling = container.child(indexInParent + 1);
  if (!listTypes.includes(nextSibling.type)) return false;
  const firstItem = nextSibling.firstChild;
  if (!firstItem || firstItem.type !== state.schema.nodes.listItem) return false;
  const firstPara = firstItem.firstChild;
  if (!firstPara || firstPara.type !== state.schema.nodes.paragraph) return false;

  if (dispatch) {
    let tr = state.tr;
    if (nextSibling.childCount === 1) {
      // Single-item list — remove the whole list node
      tr = tr.delete(curParaContentEnd + 1, curParaContentEnd + 1 + nextSibling.nodeSize);
    } else {
      // Multi-item list — remove only firstItem (including any nested content it may contain)
      tr = tr.delete(curParaContentEnd + 2, curParaContentEnd + 2 + firstItem.nodeSize);
    }
    tr = tr.insert(curParaContentEnd, firstPara.content);
    // Keep cursor at its original position (not at end of inserted content).
    tr = tr.setSelection(TextSelection.create(tr.doc, curParaContentEnd));
    dispatch(tr.scrollIntoView());
  }
  return true;
};

// Handles the case where cursor is at the start of a paragraph that immediately follows a
// list. Without this, joinBackward's deleteBarrier re-wraps the paragraph back into the list.
// Instead, we merge the paragraph's content into the last item's paragraph.
export const joinAfterList = (state: EditorState, dispatch?: (tr: Transaction) => void): boolean => {
  const $cursor = (state.selection as any).$cursor;
  if (!$cursor || $cursor.parentOffset !== 0) return false;

  const depth = $cursor.depth;
  // Not in a list item (listBackspace handles that case)
  if (depth >= 1 && $cursor.node(depth - 1).type === state.schema.nodes.listItem) return false;

  // Check if the sibling immediately before the current paragraph is a list
  const indexInParent = $cursor.index(depth - 1);
  if (indexInParent === 0) return false;

  const grandParent = $cursor.node(depth - 1);
  const prevSibling = grandParent.child(indexInParent - 1);
  const listTypes = [state.schema.nodes.bulletList, state.schema.nodes.orderedList];
  if (!listTypes.includes(prevSibling.type)) return false;

  const lastItem = prevSibling.lastChild;
  if (!lastItem || lastItem.type !== state.schema.nodes.listItem) return false;

  // If the current paragraph is empty, just delete it entirely (regardless of list nesting depth).
  // Place cursor at the end of the previous line (last text position before the deleted paragraph).
  if ($cursor.parent.content.size === 0) {
    if (dispatch) {
      const curParaStart = $cursor.before(depth);
      const curParaEnd = $cursor.after(depth);
      const tr = state.tr.delete(curParaStart, curParaEnd);
      // curParaStart still exists in the new doc; search backward for nearest text position.
      const sel = TextSelection.near(tr.doc.resolve(curParaStart), -1);
      tr.setSelection(sel);
      dispatch(tr.scrollIntoView());
    }
    return true;
  }

  // Only handle the simple case: lastItem has exactly one child (a paragraph).
  // If there are nested lists, fall through to joinBackward.
  if (lastItem.childCount !== 1) return false;
  const lastPara = lastItem.firstChild;
  if (!lastPara || lastPara.type !== state.schema.nodes.paragraph) return false;

  if (dispatch) {
    // Position before the opening <p> of the current paragraph.
    // In the token sequence: ... </lastPara> </lastItem> </list> [curParaStart] <p> [curParaContentStart] ...
    const curParaStart = $cursor.pos - 1; // = $cursor.before($cursor.depth)
    const curParaContentStart = $cursor.pos;

    // End of lastPara's content, computed using node sizes.
    // The token sequence before curParaStart is: ... </lastPara> </lastItem> </list>
    // lastItem ends at position (curParaStart - 1) inside the list, so:
    //   lastItemStart = curParaStart - 1 - lastItem.nodeSize
    //   lastPara starts at lastItemStart + 1
    //   lastPara content ends at lastItemStart + 1 + lastPara.nodeSize - 1
    //                         = curParaStart - 1 - lastItem.nodeSize + lastPara.nodeSize
    const lastParaContentEnd = curParaStart - 1 - lastItem.nodeSize + lastPara.nodeSize;

    // Delete from the end of lastPara's content to the start of the current para's content.
    // This removes: </lastPara> </lastItem> </list> <p>  (the structural boundary)
    // and makes lastPara's content and curPara's content adjacent in the same paragraph.
    const tr = state.tr.delete(lastParaContentEnd, curParaContentStart);
    dispatch(tr.scrollIntoView());
  }
  return true;
};

function isInList(state: EditorState, listType: NodeType): boolean {
  const { $from } = state.selection;
  for (let d = $from.depth; d >= 0; d--) {
    if ($from.node(d).type === listType) return true;
  }
  return false;
}

export const toggleBulletList = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  if (isInList(state, state.schema.nodes.bulletList)) {
    return liftListItem(state.schema.nodes.listItem)(state, dispatch);
  }
  return wrapInList(state.schema.nodes.bulletList)(state, dispatch);
};

export const toggleOrderedList = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  if (isInList(state, state.schema.nodes.orderedList)) {
    return liftListItem(state.schema.nodes.listItem)(state, dispatch);
  }
  return wrapInList(state.schema.nodes.orderedList)(state, dispatch);
};

export const makeListInputRules = (schema: any) =>
  inputRules({
    rules: [
      wrappingInputRule(/^\s*[-]\s$/, schema.nodes.bulletList),
      wrappingInputRule(
        /^(\d+)\.\s$/,
        schema.nodes.orderedList,
        (match: RegExpMatchArray) => ({ order: +match[1] }),
        (match: RegExpMatchArray, node: any) => node.childCount + node.attrs.order === +match[1]
      ),
    ],
  });

export { splitListItem, sinkListItem, liftListItem } from "prosemirror-schema-list";
