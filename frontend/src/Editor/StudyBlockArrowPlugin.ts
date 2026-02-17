import { EditorState, Selection, TextSelection, Transaction } from "prosemirror-state";
import { EditorView } from "prosemirror-view";
import { Node } from "prosemirror-model";
import { keymap } from "prosemirror-keymap";

// Handles ArrowUp/ArrowDown at study block cell boundaries.
// ProseMirror's default navigation follows document order, which causes the
// cursor to jump to the wrong cell in the two-column table layout. This
// plugin intercepts at cell boundaries and navigates visually instead.

const studyBlockCellTypes = new Set([
  "generalStudyBlockHeader",
  "generalStudyBlockBody",
  "questionText",
  "questionAnswer",
]);

function findCellInfo(state: EditorState) {
  const { $head } = state.selection;
  for (let d = $head.depth; d > 0; d--) {
    if (studyBlockCellTypes.has($head.node(d).type.name)) {
      return { depth: d, start: $head.before(d), end: $head.after(d) };
    }
  }
  return null;
}

// Helper: calculate the document position of the nth child of a node,
// given the node's content-start position.
function childPos(parent: Node, contentStart: number, childIndex: number): number {
  let pos = contentStart;
  for (let i = 0; i < childIndex; i++) {
    pos += parent.child(i).nodeSize;
  }
  return pos;
}

// Resolves a target cursor position on the same visual line as anchorPos,
// preserving cursorLeft for horizontal placement. Falls back to
// Selection.near(anchorPos) if the probe lands outside [rangeStart, rangeEnd].
function resolveLinePos(
  state: EditorState,
  view: EditorView,
  cursorLeft: number,
  anchorPos: number,
  nearDir: -1 | 1,
  rangeStart: number,
  rangeEnd: number
): number {
  const nearSel = Selection.near(state.doc.resolve(anchorPos), nearDir);
  const targetCoords = view.coordsAtPos(nearSel.head);
  const hit = view.posAtCoords({ left: cursorLeft, top: targetCoords.top });
  if (hit && hit.pos >= rangeStart && hit.pos <= rangeEnd) {
    const $hit = state.doc.resolve(hit.pos);
    if ($hit.parent.inlineContent) return hit.pos;
    // hit.pos landed on a boundary between block nodes — snap into the range
    const snapped = Selection.near($hit, 1);
    if (snapped.head > rangeStart && snapped.head < rangeEnd) return snapped.head;
    const snappedBack = Selection.near($hit, -1);
    if (snappedBack.head > rangeStart && snappedBack.head < rangeEnd) return snappedBack.head;
  }
  return nearSel.head;
}

function dispatchToLine(
  state: EditorState,
  dispatch: (tr: Transaction) => void,
  view: EditorView,
  cursorLeft: number,
  anchorPos: number,
  nearDir: -1 | 1,
  rangeStart: number,
  rangeEnd: number
): void {
  const newPos = resolveLinePos(state, view, cursorLeft, anchorPos, nearDir, rangeStart, rangeEnd);
  dispatch(state.tr.setSelection(TextSelection.create(state.doc, newPos)).scrollIntoView());
}

function findAdjacentCell(
  state: EditorState,
  cell: { depth: number; start: number; end: number },
  dir: -1 | 1
): { start: number; end: number } | null {
  const { $head } = state.selection;
  const cellTypeName = $head.node(cell.depth).type.name;

  // --- generalStudyBlock cells ---
  if (cellTypeName === "generalStudyBlockHeader" || cellTypeName === "generalStudyBlockBody") {
    const rowDepth = cell.depth - 1;
    const containerDepth = rowDepth - 1;
    if (containerDepth < 0) return null;

    const cellIndex = $head.index(rowDepth);
    const rowIndex = $head.index(containerDepth);
    const container = $head.node(containerDepth);

    const targetRowIndex = rowIndex + dir;
    if (targetRowIndex < 0 || targetRowIndex >= container.childCount) return null;

    const targetRow = container.child(targetRowIndex);
    const targetRowPos = childPos(container, $head.start(containerDepth), targetRowIndex);

    if (targetRow.type.name === "generalStudyBlock") {
      const idx = Math.min(cellIndex, targetRow.childCount - 1);
      const targetCellPos = childPos(targetRow, targetRowPos + 1, idx);
      return { start: targetCellPos, end: targetCellPos + targetRow.child(idx).nodeSize };
    }

    if (targetRow.type.name === "questions" && targetRow.childCount > 0) {
      if (dir === -1) {
        // Going UP: last child of the last question
        const lastQ = targetRow.child(targetRow.childCount - 1);
        const lastQPos = childPos(targetRow, targetRowPos + 1, targetRow.childCount - 1);
        const lastChildIdx = lastQ.childCount - 1;
        const cellStart = childPos(lastQ, lastQPos + 1, lastChildIdx);
        return { start: cellStart, end: cellStart + lastQ.child(lastChildIdx).nodeSize };
      } else {
        // Going DOWN: first child of the first question
        const firstQ = targetRow.child(0);
        const firstQPos = targetRowPos + 1; // enter questions
        const cellStart = firstQPos + 1; // enter first question
        return { start: cellStart, end: cellStart + firstQ.child(0).nodeSize };
      }
    }

    return null;
  }

  // --- questionText / questionAnswer cells ---
  if (cellTypeName === "questionText" || cellTypeName === "questionAnswer") {
    const questionDepth = cell.depth - 1;
    const questionsDepth = questionDepth - 1;
    const containerDepth = questionsDepth - 1;
    if (containerDepth < 0) return null;

    const question = $head.node(questionDepth);
    const questions = $head.node(questionsDepth);
    const container = $head.node(containerDepth);
    const cellIdx = $head.index(questionDepth);
    const questionIdx = $head.index(questionsDepth);
    const questionsRowIdx = $head.index(containerDepth);

    if (dir === -1) { // ArrowUp
      // Previous sibling within the same question
      if (cellIdx > 0) {
        const prev = question.child(cellIdx - 1);
        const prevPos = childPos(question, $head.start(questionDepth), cellIdx - 1);
        return { start: prevPos, end: prevPos + prev.nodeSize };
      }
      // First child — go to last child of previous question
      if (questionIdx > 0) {
        const prevQ = questions.child(questionIdx - 1);
        const prevQPos = childPos(questions, $head.start(questionsDepth), questionIdx - 1);
        const lastIdx = prevQ.childCount - 1;
        const cellStart = childPos(prevQ, prevQPos + 1, lastIdx);
        return { start: cellStart, end: cellStart + prevQ.child(lastIdx).nodeSize };
      }
      // First child of first question — navigate out of studyBlocks
      return null;
    }

    if (dir === 1) { // ArrowDown
      // Next sibling within the same question
      if (cellIdx < question.childCount - 1) {
        const next = question.child(cellIdx + 1);
        const nextPos = childPos(question, $head.start(questionDepth), cellIdx + 1);
        return { start: nextPos, end: nextPos + next.nodeSize };
      }
      // Last child — go to first child of next question
      if (questionIdx < questions.childCount - 1) {
        const nextQ = questions.child(questionIdx + 1);
        const nextQPos = childPos(questions, $head.start(questionsDepth), questionIdx + 1);
        const cellStart = nextQPos + 1; // enter next question
        return { start: cellStart, end: cellStart + nextQ.child(0).nodeSize };
      }
      // Last child of last question — go to next row in studyBlocks
      if (questionsRowIdx < container.childCount - 1) {
        const nextRow = container.child(questionsRowIdx + 1);
        const nextRowPos = childPos(container, $head.start(containerDepth), questionsRowIdx + 1);
        if (nextRow.type.name === "generalStudyBlock" && nextRow.childCount >= 2) {
          // Go to body (index 1) since questions content is in the right column
          const bodyPos = childPos(nextRow, nextRowPos + 1, 1);
          return { start: bodyPos, end: bodyPos + nextRow.child(1).nodeSize };
        }
      }
      return null;
    }
  }

  return null;
}

// Returns whether the cursor is on the first (dir=-1) or last (dir=1) line of
// its cell. Uses Selection.near for robust detection in stretched table cells.
function atCellEdge(
  headTop: number,
  state: EditorState,
  view: EditorView,
  cell: { start: number; end: number },
  dir: -1 | 1
): boolean {
  if (dir === -1) {
    const firstSel = Selection.near(state.doc.resolve(cell.start + 1), 1);
    if (firstSel.head >= cell.start && firstSel.head <= cell.end) {
      return Math.abs(headTop - view.coordsAtPos(firstSel.head).top) < 5;
    }
  } else {
    const lastSel = Selection.near(state.doc.resolve(cell.end - 1), -1);
    if (lastSel.head >= cell.start && lastSel.head <= cell.end) {
      return Math.abs(headTop - view.coordsAtPos(lastSel.head).top) < 5;
    }
  }
  return true; // empty cell — always at edge
}

function navigateToTarget(
  state: EditorState,
  dispatch: ((tr: Transaction) => void) | undefined,
  view: EditorView,
  cursorLeft: number,
  target: { start: number; end: number },
  dir: -1 | 1
): boolean {
  if (!dispatch) return true;
  const anchorPos = dir === -1 ? target.end - 1 : target.start + 1;
  const newPos = resolveLinePos(state, view, cursorLeft, anchorPos, dir === -1 ? -1 : 1, target.start, target.end);
  if (newPos === state.selection.head) return false;
  dispatch(state.tr.setSelection(TextSelection.create(state.doc, newPos)).scrollIntoView());
  return true;
}

// Navigates out of a studyBlocks node when there is no adjacent cell in dir.
// Goes to the previous sibling (bibleText) or next section's first content,
// preserving horizontal cursor position via coordinate probing.
function navigateOutOfStudyBlocks(
  state: EditorState,
  dispatch: ((tr: Transaction) => void) | undefined,
  view: EditorView,
  dir: -1 | 1,
  cursorLeft: number
): boolean {
  const { $head } = state.selection;
  for (let d = $head.depth; d > 0; d--) {
    if ($head.node(d).type.name !== "studyBlocks") continue;
    const sectionDepth = d - 1;

    if (dir === -1 && sectionDepth >= 0) {
      const section = $head.node(sectionDepth);
      const studyBlocksIdx = $head.index(sectionDepth);
      if (studyBlocksIdx > 0) {
        const prevSib = section.child(studyBlocksIdx - 1);
        const prevSibPos = childPos(section, $head.start(sectionDepth), studyBlocksIdx - 1);
        const prevSibEnd = prevSibPos + prevSib.nodeSize;
        if (dispatch) dispatchToLine(state, dispatch, view, cursorLeft, prevSibEnd - 1, -1, prevSibPos, prevSibEnd);
        return true;
      }
    }

    if (dir === 1) {
      const docDepth = sectionDepth - 1;
      if (docDepth >= 0) {
        const doc = $head.node(docDepth);
        const sectionIdx = $head.index(docDepth);
        if (sectionIdx < doc.childCount - 1) {
          const nextSection = doc.child(sectionIdx + 1);
          const nextSectionPos = childPos(doc, $head.start(docDepth), sectionIdx + 1);
          const nextSectionEnd = nextSectionPos + nextSection.nodeSize;
          if (dispatch) dispatchToLine(state, dispatch, view, cursorLeft, nextSectionPos + 1, 1, nextSectionPos, nextSectionEnd);
          return true;
        }
      }
    }

    // Fallback: structural navigation via Selection.near
    if (dispatch) {
      const boundaryPos = dir === -1 ? $head.before(d) : $head.after(d);
      const $boundary = state.doc.resolve(boundaryPos);
      dispatch(state.tr.setSelection(Selection.near($boundary, dir) as TextSelection).scrollIntoView());
    }
    return true;
  }

  return false;
}

// Handles ArrowUp from a sectionHeader into the studyBlocks of the previous section.
// ProseMirror's default would go to the end of the studyBlocks body (right column),
// but the user expects to land in whichever column their X position aligns with.
function handleSectionHeaderUp(
  state: EditorState,
  dispatch: ((tr: Transaction) => void) | undefined,
  view: EditorView
): boolean {
  if (!(state.selection instanceof TextSelection)) return false;
  const { $head } = state.selection;

  let headerDepth = -1;
  for (let d = $head.depth; d > 0; d--) {
    if ($head.node(d).type.name === "sectionHeader") { headerDepth = d; break; }
  }
  if (headerDepth < 0) return false;

  // Only trigger at the top edge (first line) of the sectionHeader
  const headCoords = view.coordsAtPos(state.selection.head);
  const firstPosCoords = view.coordsAtPos($head.start(headerDepth));
  if (Math.abs(headCoords.top - firstPosCoords.top) > 5) return false;

  const sectionDepth = headerDepth - 1;
  const docDepth = sectionDepth - 1;
  if (docDepth < 0) return false;

  const docNode = $head.node(docDepth);
  const sectionIdx = $head.index(docDepth);
  if (sectionIdx === 0) return false;

  const prevSection = docNode.child(sectionIdx - 1);
  if (prevSection.childCount === 0) return false;

  const lastChild = prevSection.child(prevSection.childCount - 1);
  if (lastChild.type.name !== "studyBlocks") return false;

  const prevSectionPos = childPos(docNode, $head.start(docDepth), sectionIdx - 1);
  const sbPos = childPos(prevSection, prevSectionPos + 1, prevSection.childCount - 1);
  const sbEnd = sbPos + lastChild.nodeSize;

  if (dispatch) dispatchToLine(state, dispatch, view, headCoords.left, sbEnd - 1, -1, sbPos, sbEnd);
  return true;
}

function handleStudyBlockArrow(
  dir: -1 | 1,
  state: EditorState,
  dispatch: ((tr: Transaction) => void) | undefined,
  view: EditorView | undefined
): boolean {
  if (!view || !(state.selection instanceof TextSelection)) return false;

  const cell = findCellInfo(state);
  if (!cell) return false;

  const headCoords = view.coordsAtPos(state.selection.head);
  if (!atCellEdge(headCoords.top, state, view, cell, dir)) return false;

  const target = findAdjacentCell(state, cell, dir);
  if (target) return navigateToTarget(state, dispatch, view, headCoords.left, target, dir);
  return navigateOutOfStudyBlocks(state, dispatch, view, dir, headCoords.left);
}

export const studyBlockArrowPlugin = keymap({
  ArrowUp: (state, dispatch, view) => {
    if (view && handleSectionHeaderUp(state, dispatch, view)) return true;
    return handleStudyBlockArrow(-1, state, dispatch, view);
  },
  ArrowDown: (state, dispatch, view) => handleStudyBlockArrow(1, state, dispatch, view),
});
