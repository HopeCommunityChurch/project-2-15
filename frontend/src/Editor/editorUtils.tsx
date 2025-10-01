import { EditorState, Transaction, TextSelection } from "prosemirror-state";
import { toggleMark } from "prosemirror-commands";
import { Schema } from "prosemirror-model";
import { textSchema } from "./textSchema";
import { Slice, Node, Mark, Fragment } from "prosemirror-model";
import { v4 as uuidv4 } from "uuid";
import * as phistory from "prosemirror-history";


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

// Use the order of the array to say what the new order is
export const reorderStudyBlock = (
  pos: number,
  changes : Array<Node>
) => (
  state: EditorState,
  dispatch?: (tr: Transaction) => void
) => {
  let sectionNode : Node = null;
  let sectionPos = null;
  console.log(pos);
  state.doc.nodesBetween(pos, pos, (node, pos) => {
    if (node.type.name === "section") {
      sectionNode = node;
      sectionPos = pos;
      return true;
    }
  });
  console.log(sectionNode);
  if (sectionNode === null) {
    console.error("unable to get section for editing study block");
    return;
  }
  let studyBlockNode = null
  sectionNode.descendants((node, pos, parent) => {
    if(parent.type.name === "section" && node.type.name === "studyBlocks") {
      studyBlockNode = {node, pos};
      return false;
    }
    return false;
  });
  console.log(studyBlockNode);

  let tr = state.tr;
  let delPos = studyBlockNode.pos + sectionPos;
  console.log(delPos);
  tr.deleteRange(delPos+1, delPos + studyBlockNode.node.nodeSize-1);

  let currentPos = delPos + 1;
  changes.forEach((node) => {
    tr.insert(currentPos, node);
    currentPos += node.nodeSize;
  });

  dispatch(tr)
  return true;

};
