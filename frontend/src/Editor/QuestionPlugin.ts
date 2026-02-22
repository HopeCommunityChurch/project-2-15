import {
  EditorState,
  Plugin,
  Transaction,
  TextSelection,
} from "prosemirror-state";
import { EditorView, NodeView, DecorationSet, Decoration } from "prosemirror-view";
import { Node, Mark } from "prosemirror-model";
import { StepMap } from "prosemirror-transform";
import { history, undo, redo } from "prosemirror-history";
import { keymap } from "prosemirror-keymap";
import { baseKeymap } from "prosemirror-commands";
import {
  toggleBold,
  toggleUnderline,
  toggleItalic,
  increaseLevel,
  decreaseLevel,
} from "./editorUtils";
import { textSchema } from "./textSchema";
import { highlighQuestion, unhighlighQuestion } from "./QuestionHighlightPlugin";
import { getRandomStr } from "../Util";

export interface QuestionMapItem {
  node: Node;
  getPos: () => number;
  editor: EditorView | null;
}

type notUndefined = string | number | boolean | symbol | object;

export interface Dictionary<T extends notUndefined = notUndefined> {
  [key: string]: T | undefined;
}

export function newQuestionNode(): [string, Node] {
  const questionId = getRandomStr();
  const p = textSchema.nodes.paragraph.create();
  const questionText = textSchema.nodes.questionText.create({}, p);
  const result = textSchema.nodes.question.create({ questionId }, questionText);
  return [questionId, result];
}

export const newQuestionAnswerNode = () => {
  const p = textSchema.nodes.paragraph.create();
  const questionAnswer = textSchema.nodes.questionAnswer.create({}, p);
  return questionAnswer;
};

export class QuestionsView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  node: Node;
  questionCount: number;
  constructor(node: Node, view: EditorView, getPos: () => number) {
    this.node = node;
    this.dom = document.createElement("tr");
    this.dom.className = "questions";

    const header = document.createElement("td");
    header.setAttribute("contenteditable", "false");

    const headerDiv = document.createElement("div");
    headerDiv.setAttribute("contenteditable", "false");
    headerDiv.innerText = "Questions";
    headerDiv.className = "studyBlockHeaderDiv";

    headerDiv.onclick = () => {
      const transaction = view.state.tr.setSelection(
        TextSelection.near(view.state.doc.resolve(getPos() - 3))
      );
      view.dispatch(transaction);
      view.focus();
    };
    header.appendChild(headerDiv);
    this.dom.appendChild(header);

    this.contentDOM = document.createElement("td");

    if (node.content.size === 0) {
      const noQuestionsText = document.createElement("div");
      noQuestionsText.className = "noQuestionsText";
      noQuestionsText.setAttribute("contenteditable", "false");
      const em1 = document.createElement("em");
      em1.textContent = 'Insert a question by selecting some text and clicking the "Add Question" button';
      const icon = document.createElement("img");
      icon.src = window.base + "/static/img/question-icon.svg";
      icon.alt = "Add Question Icon";
      const em2 = document.createElement("em");
      em2.textContent = "in the toolbar above";
      noQuestionsText.appendChild(em1);
      noQuestionsText.appendChild(document.createTextNode(" "));
      noQuestionsText.appendChild(icon);
      noQuestionsText.appendChild(document.createTextNode(" "));
      noQuestionsText.appendChild(em2);

      noQuestionsText.onclick = () => {
        const transaction = view.state.tr.setSelection(
          TextSelection.near(view.state.doc.resolve(getPos() - 3))
        );
        view.dispatch(transaction);
        view.focus();
      };

      this.contentDOM.appendChild(noQuestionsText);
    }

    this.dom.appendChild(this.contentDOM);
  }
  update(node: Node) {
    this.node = node;
    return true;
  }
}

type VerseSeg = {
  verse: { book: string; chapter: number; verse: number } | null;
  texts: Array<{ text: string; selected: boolean }>;
};

function getQuestionContext(questionId: string, view: EditorView): VerseSeg[] | null {
  const verseSegs: VerseSeg[] = [];
  let currentVerseKey: string | null = null;

  view.state.doc.descendants((node) => {
    if (node.type.name === "section") return true;
    if (node.type.name === "bibleText") return true;
    if (node.type.name === "chunk") return true;
    if (node.type.name !== "text") return false;

    const verse = node.marks.find((m) => m.type.name === "verse");
    const isSelected = !!node.marks.find(
      (m) => m.type.name === "questionReference" && m.attrs.questionId === questionId
    );

    if (!verse) return false;

    const verseKey = `${verse.attrs.book}-${verse.attrs.chapter}-${verse.attrs.verse}`;

    if (verseKey !== currentVerseKey) {
      verseSegs.push({ verse: verse.attrs, texts: [{ text: node.text, selected: isSelected }] });
      currentVerseKey = verseKey;
    } else {
      verseSegs[verseSegs.length - 1].texts.push({ text: node.text, selected: isSelected });
    }
    return false;
  });

  const firstSelected = verseSegs.findIndex((seg) => seg.texts.some((t) => t.selected));
  const lastSelected = verseSegs.reduce((acc, seg, i) => (seg.texts.some((t) => t.selected) ? i : acc), -1);

  if (firstSelected === -1) return null;

  const selectedBook = verseSegs[firstSelected].verse?.book;

  const prevIdx = firstSelected - 1;
  const start = (prevIdx >= 0 && verseSegs[prevIdx].verse?.book === selectedBook)
    ? prevIdx
    : firstSelected;

  const nextIdx = lastSelected + 1;
  const end = (nextIdx < verseSegs.length && verseSegs[nextIdx].verse?.book === selectedBook)
    ? nextIdx
    : lastSelected;

  return verseSegs.slice(start, end + 1);
}

function showVerseContextPopover(anchor: HTMLElement, questionId: string, view: EditorView) {
  const existingId = "verse-context-popover-" + questionId;
  const existing = document.getElementById(existingId);
  if (existing) {
    existing.remove();
    return;
  }

  const context = getQuestionContext(questionId, view);
  if (!context) return;

  const popover = document.createElement("div");
  popover.id = existingId;
  popover.className = "verseContextPopover";

  context.forEach((seg) => {
    const line = document.createElement("div");
    const isLineSelected = seg.texts.some((t) => t.selected);
    line.className = "verseContextLine" + (isLineSelected ? " verseContextLineSelected" : "");

    if (seg.verse) {
      const verseNum = document.createElement("span");
      verseNum.className = "verseContextNum";
      verseNum.textContent = `${seg.verse.chapter}:${seg.verse.verse}`;
      line.appendChild(verseNum);
    }

    const textSpan = document.createElement("span");
    seg.texts.forEach((t) => {
      if (t.selected) {
        const hl = document.createElement("mark");
        hl.className = "verseContextHighlight";
        hl.textContent = t.text;
        textSpan.appendChild(hl);
      } else {
        textSpan.appendChild(document.createTextNode(t.text));
      }
    });
    line.appendChild(textSpan);

    popover.appendChild(line);
  });

  const container = document.getElementById("mainEditorHolder") || document.body;

  popover.style.position = "absolute";
  popover.style.visibility = "hidden";
  container.appendChild(popover);

  const anchorRect = anchor.getBoundingClientRect();
  const containerRect = container.getBoundingClientRect();
  const popRect = popover.getBoundingClientRect();
  const viewportH = window.innerHeight;
  const viewportW = window.innerWidth;

  // Convert viewport coords to coords within the scrolling container
  const anchorTopInContainer = anchorRect.top - containerRect.top + container.scrollTop;
  const anchorBottomInContainer = anchorRect.bottom - containerRect.top + container.scrollTop;
  const anchorLeftInContainer = anchorRect.left - containerRect.left + container.scrollLeft;

  let top: number;
  if (anchorRect.top - 6 - popRect.height >= 10) {
    top = anchorTopInContainer - popRect.height - 6;
  } else {
    top = anchorBottomInContainer + 6;
  }

  let left = anchorLeftInContainer;
  if (anchorRect.left + popRect.width > viewportW - 10) {
    left = anchorLeftInContainer - (anchorRect.left + popRect.width - (viewportW - 10));
  }
  if (left < 0) left = 0;

  popover.style.top = top + "px";
  popover.style.left = left + "px";
  popover.style.visibility = "visible";

  const closeHandler = (e: MouseEvent) => {
    if (!popover.contains(e.target as Node) && e.target !== anchor) {
      popover.remove();
      document.removeEventListener("mousedown", closeHandler);
    }
  };
  setTimeout(() => document.addEventListener("mousedown", closeHandler), 0);
}

function formatBibleReference(verse1: any, verse2: any): string {
  if (verse1 == null || verse2 == null) return "Q";
  if (verse1.book !== verse2.book) {
    return "Q";
  }
  if (verse1.chapter === verse2.chapter && verse1.verse === verse2.verse) {
    return `${verse1.chapter}:${verse1.verse}`;
  }
  if (verse1.chapter === verse2.chapter) {
    return `${verse1.chapter}:${verse1.verse}-${verse2.verse}`;
  }
  return `${verse1.chapter}:${verse1.verse}-${verse2.chapter}:${verse2.verse}`;
}

export class QuestionView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  questionId: string;
  node: Node;
  questionMap: Dictionary<QuestionMapItem>;

  constructor(
    questionMap: Dictionary<QuestionMapItem>,
    node: Node,
    view: EditorView,
    getPos: () => number
  ) {
    this.node = node;
    this.questionId = node.attrs.questionId;

    let vStart = null;
    let vEnd = null;

    view.state.doc.descendants((node) => {
      if (node.type.name === "section") return true;
      if (node.type.name === "bibleText") return true;
      if (node.type.name === "chunk") return true;
      if (node.type.name !== "text") return false;

      const isQuestion = node.marks.find(
        (m) => m.type.name === "questionReference" && m.attrs.questionId === this.questionId
      );
      if (isQuestion == null) return false;
      const verse = node.marks.find((m) => m.type.name === "verse");
      if (verse == null) return false;

      if (vStart == null) vStart = verse.attrs;
      vEnd = verse.attrs;
    });

    this.questionMap = questionMap;
    if (!questionMap[this.questionId]) {
      questionMap[this.questionId] = {
        node: node,
        getPos: getPos,
        editor: null,
      };
    } else {
      // On remount (e.g. after undo), refresh node and getPos so dispatchInner
      // always calls the live position callback rather than a stale one.
      questionMap[this.questionId].node = node;
      questionMap[this.questionId].getPos = getPos;
    }

    this.dom = document.createElement("questionOuter");
    const qtext = document.createElement("div");
    qtext.setAttribute("contenteditable", "false");
    qtext.innerText = formatBibleReference(vStart, vEnd);
    if (vStart !== null) {
      qtext.classList.add("verseRefClickable");
      qtext.title = "View passage context";
      qtext.onclick = (e) => {
        e.stopPropagation();
        showVerseContextPopover(qtext, this.questionId, view);
      };
    }
    this.dom.appendChild(qtext);
    this.contentDOM = document.createElement("question");
    this.dom.appendChild(this.contentDOM);

    if (view.editable) {
      const addAnswer = document.createElement("button");
      addAnswer.onclick = (e) => {
        e.preventDefault();
        const qnode = newQuestionAnswerNode();
        const length = this.node.content.size;
        const pos = getPos() + length;
        const tr1 = view.state.tr.insert(pos + 1, qnode);
        const sel = TextSelection.create(tr1.doc, pos + 3);
        const tr2 = tr1.setSelection(sel);
        view.dispatch(tr2);
      };
      addAnswer.innerText = "+ Answer";
      addAnswer.className = "addAnswer";
      this.dom.appendChild(addAnswer);
    }
  }

  update(node: Node) {
    this.node = node;
    if (this.questionMap) {
      this.questionMap[this.questionId].node = node;
      let innerView = this.questionMap[this.questionId].editor;
      if (innerView) {
        let state = innerView.state;
        let start = node.content.findDiffStart(state.doc.content);
        if (start != null) {
          let { a: endA, b: endB } = node.content.findDiffEnd(state.doc.content);
          let overlap = start - Math.min(endA, endB);
          if (overlap > 0) {
            endA += overlap;
            endB += overlap;
          }
          innerView.dispatch(
            state.tr.replace(start, endB, node.slice(start, endA)).setMeta("fromOutside", true)
          );
        }
      }
    }
    return true;
  }
}

export class QuestionAnswerView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  constructor(node: Node) {
    this.dom = document.createElement("questionanswerouter");
    const qtext = document.createElement("div");
    qtext.setAttribute("contenteditable", "false");
    qtext.innerText = "A:";
    this.dom.appendChild(qtext);
    this.contentDOM = document.createElement("questionanswer");
    this.dom.appendChild(this.contentDOM);
  }
  update() {
    return true;
  }
}

export const questionReferenceMarkView = (mark: Mark, view: EditorView) => {
  const mview = document.createElement("questionRef");
  mview.className = "questionRef";
  const qId = mark.attrs.questionId;
  mview.setAttribute("questionId", qId);
  return { dom: mview };
};

export const referenceToMarkView = (mark: Mark, view: EditorView) => {
  const mview = document.createElement("span");
  const rId = mark.attrs.referenceId;
  mview.setAttribute("referenceId", rId);
  return { dom: mview };
};

const zIndices: { [key: string]: number } = {};
let zIndexCounter: number = 17;

export const questionPopup = (
  x: number,
  y: number,
  qId: string,
  questionMap: Dictionary<QuestionMapItem>,
  view: EditorView,
  setCurrentEditor: (view: EditorView) => void
) => {
  let qNode = questionMap[qId];
  if (!qNode.editor) {
    const pop = document.createElement("questionRefPopup");
    pop.className = "questionRefPopup";

    pop.style.position = "fixed";
    pop.style.left = "-9999px";
    pop.style.top = "-9999px";

    const p215EditorHolder = document.getElementById("editorHolder");
    p215EditorHolder.appendChild(pop);

    const rect = pop.getBoundingClientRect();
    const windowWidth = window.innerWidth;
    const windowHeight = window.innerHeight;

    // pageX/pageY equal clientX/clientY here since only #mainEditorHolder scrolls, not the page itself
    let initialLeft = x - 20;
    let initialTop = y + parseInt(getComputedStyle(document.documentElement).fontSize);

    if (initialLeft + rect.width > windowWidth) {
      initialLeft = windowWidth - rect.width;
    }
    if (initialTop + rect.height > windowHeight) {
      initialTop = windowHeight - rect.height;
    }

    pop.style.left = initialLeft + "px";
    pop.style.top = initialTop + "px";
    pop.style.visibility = "visible";

    const clampToViewport = () => {
      const vw = window.innerWidth;
      const vh = window.innerHeight;
      const r = pop.getBoundingClientRect();

      // Account for border/padding when shrinking to viewport
      const borderW = pop.offsetWidth - pop.clientWidth;
      const borderH = pop.offsetHeight - pop.clientHeight;
      if (r.width > vw) pop.style.width = (vw - borderW) + "px";
      if (r.height > vh) pop.style.height = (vh - borderH) + "px";

      const r2 = pop.getBoundingClientRect();
      // Use getBoundingClientRect for position since style.left may be "calc(...)" after drag
      let l = r2.left;
      let t = r2.top;
      if (l + r2.width > vw) l = vw - r2.width;
      if (t + r2.height > vh) t = vh - r2.height;
      if (l < 0) l = 0;
      if (t < 0) t = 0;
      pop.style.left = l + "px";
      pop.style.top = t + "px";
    };

    window.addEventListener("resize", clampToViewport);

    if (!zIndices[qId]) {
      zIndices[qId] = zIndexCounter++;
    }
    pop.style.zIndex = zIndices[qId].toString();

    pop.addEventListener("mousedown", () => {
      const currentZIndex = zIndices[qId];
      const highestZIndex = Math.max(...Object.values(zIndices));
      if (currentZIndex < highestZIndex) {
        zIndices[qId] = highestZIndex + 1;
        pop.style.zIndex = zIndices[qId].toString();
      }
    });

    highlighQuestion(qId, view.state, view.dispatch);

    let mover = pop.appendChild(document.createElement("mover"));

    const startDrag = (clientX, clientY) => {
      const rec = pop.getBoundingClientRect();
      const diffX = clientX - rec.x;
      const diffY = clientY - rec.y;

      const moveDrag = (clientX, clientY) => {
        let newX = clientX - diffX;
        let newY = clientY - diffY;

        const windowWidth = window.innerWidth;
        const windowHeight = window.innerHeight;
        const rect = pop.getBoundingClientRect();
        if (newX < 0) newX = 0;
        if (newY < 0) newY = 0;
        if (newX + rect.width > windowWidth) newX = windowWidth - rect.width;
        if (newY + rect.height > windowHeight) newY = windowHeight - rect.height;

        pop.style.left = "calc(" + newX + "px)";
        pop.style.top = "calc(" + newY + "px)";
      };

      const mousemove = (e) => {
        e.preventDefault();
        moveDrag(e.pageX, e.pageY);
      };

      const touchmove = (e) => {
        e.preventDefault();
        const touch = e.touches[0];
        moveDrag(touch.pageX, touch.pageY);
      };

      document.addEventListener("mousemove", mousemove);
      document.addEventListener("mouseup", () => {
        document.removeEventListener("mousemove", mousemove);
      });

      document.addEventListener("touchmove", touchmove);
      document.addEventListener("touchend", () => {
        document.removeEventListener("touchmove", touchmove);
      });
    };

    mover.addEventListener("mousedown", (e) => {
      e.preventDefault();
      startDrag(e.pageX, e.pageY);
    });

    mover.addEventListener("touchstart", (e) => {
      e.preventDefault();
      const touch = e.touches[0];
      startDrag(touch.pageX, touch.pageY);
    });

    const resizeHandle = pop.appendChild(document.createElement("div"));
    resizeHandle.className = "resizeHandle";

    // Purely visual overlay — no pointer events; highlights active resize edge(s)
    const edgeOverlay = pop.appendChild(document.createElement("div"));
    edgeOverlay.style.cssText =
      "position:absolute;top:0;left:0;right:0;bottom:0;pointer-events:none;z-index:999;border-radius:10px;" +
      "border-top:4px solid transparent;border-bottom:4px solid transparent;" +
      "border-left:3px solid transparent;border-right:3px solid transparent;" +
      "transition:border-color 0.15s ease;";

    let resizing = false;

    const noEdges = { top: false, bottom: false, left: false, right: false };

    const zoneEdges = (zone: HTMLDivElement) => ({
      top: zone.dataset.edge === "top" || zone.dataset.edgeV === "top",
      bottom: zone.dataset.edge === "bottom" || zone.dataset.edgeV === "bottom",
      left: zone.dataset.edge === "left" || zone.dataset.edgeH === "left",
      right: zone.dataset.edge === "right" || zone.dataset.edgeH === "right",
    });

    const updateHighlight = (edges: typeof noEdges) => {
      edgeOverlay.style.borderTopColor = edges.top ? "#ccc" : "transparent";
      edgeOverlay.style.borderBottomColor = edges.bottom ? "#ccc" : "transparent";
      edgeOverlay.style.borderLeftColor = edges.left ? "#ccc" : "transparent";
      edgeOverlay.style.borderRightColor = edges.right ? "#ccc" : "transparent";
    };

    const edgeZones: HTMLDivElement[] = [];
    for (const side of ["top", "bottom", "left", "right"] as const) {
      const zone = pop.appendChild(document.createElement("div"));
      zone.dataset.edge = side;
      const shared = "position:absolute;z-index:1000;";
      if (side === "top") zone.style.cssText = shared + "top:0;left:0;right:0;height:6px;cursor:ns-resize;";
      else if (side === "bottom") zone.style.cssText = shared + "bottom:0;left:0;right:0;height:6px;cursor:ns-resize;";
      else if (side === "left") zone.style.cssText = shared + "top:0;bottom:0;left:0;width:6px;cursor:ew-resize;";
      else zone.style.cssText = shared + "top:0;bottom:0;right:0;width:6px;cursor:ew-resize;";
      edgeZones.push(zone);
    }
    for (const corner of [["top","left"],["top","right"],["bottom","left"],["bottom","right"]] as const) {
      const zone = pop.appendChild(document.createElement("div"));
      zone.dataset.edgeV = corner[0];
      zone.dataset.edgeH = corner[1];
      const cursor = (corner[0] === "top" && corner[1] === "left") || (corner[0] === "bottom" && corner[1] === "right")
        ? "nwse-resize" : "nesw-resize";
      zone.style.cssText = `position:absolute;z-index:1001;width:6px;height:6px;${corner[0]}:0;${corner[1]}:0;cursor:${cursor};`;
      edgeZones.push(zone);
    }

    for (const zone of edgeZones) {
      zone.addEventListener("mouseenter", () => {
        if (!resizing) updateHighlight(zoneEdges(zone));
      });
      zone.addEventListener("mouseleave", () => {
        if (!resizing) updateHighlight(noEdges);
      });
      zone.addEventListener("mousedown", (e) => {
        e.preventDefault();
        e.stopPropagation();
        startResize(e.pageX, e.pageY, zoneEdges(zone));
      });
      zone.addEventListener("touchstart", (e) => {
        e.preventDefault();
        e.stopPropagation();
        const t = e.touches[0];
        startResize(t.pageX, t.pageY, zoneEdges(zone));
      });
    }

    pop.addEventListener("mouseleave", () => {
      if (!resizing) updateHighlight(noEdges);
    });

    const startResize = (
      startX: number,
      startY: number,
      edges: typeof noEdges
    ) => {
      resizing = true;
      const startW = pop.offsetWidth;
      const startH = pop.offsetHeight;
      const startL = pop.offsetLeft;
      const startT = pop.offsetTop;

      const doResize = (cx: number, cy: number) => {
        const dx = cx - startX;
        const dy = cy - startY;
        if (edges.right) pop.style.width = Math.max(250, startW + dx) + "px";
        if (edges.bottom) pop.style.height = Math.max(150, startH + dy) + "px";
        if (edges.left) {
          const newW = Math.max(250, startW - dx);
          pop.style.width = newW + "px";
          pop.style.left = startL + (startW - newW) + "px";
        }
        if (edges.top) {
          const newH = Math.max(150, startH - dy);
          pop.style.height = newH + "px";
          pop.style.top = startT + (startH - newH) + "px";
        }
      };

      const onMouseMove = (e: MouseEvent) => { e.preventDefault(); doResize(e.pageX, e.pageY); };
      const onTouchMove = (e: TouchEvent) => { e.preventDefault(); doResize(e.touches[0].pageX, e.touches[0].pageY); };
      const stop = () => {
        resizing = false;
        document.removeEventListener("mousemove", onMouseMove);
        document.removeEventListener("mouseup", stop);
        document.removeEventListener("touchmove", onTouchMove);
        document.removeEventListener("touchend", stop);
      };
      document.addEventListener("mousemove", onMouseMove);
      document.addEventListener("mouseup", stop);
      document.addEventListener("touchmove", onTouchMove);
      document.addEventListener("touchend", stop);
    };

    let popUpTitle = mover.appendChild(document.createElement("p"));
    popUpTitle.innerHTML = "Question";
    popUpTitle.className = "QpopUpTitle";

    let questionIconImg = document.createElement("img");
    questionIconImg.src = window.base + "/static/img/question-icon.svg";

    popUpTitle.prepend(questionIconImg);

    // Use a real <button> so the close control is keyboard-accessible
    const closer = mover.appendChild(document.createElement("button"));
    const closeImage = document.createElement("img");
    closer.className = "closer";
    closer.setAttribute("aria-label", "Close question popup");
    closeImage.src = window.base + "/static/img/x.svg";
    closer.appendChild(closeImage);
    const closePopup = () => {
      window.removeEventListener("resize", clampToViewport);
      qNode.editor.destroy();
      qNode.editor = null;
      pop.parentNode.removeChild(pop);
    };

    closer.onclick = (e) => {
      unhighlighQuestion(qId, view.state, view.dispatch);
      closePopup();
    };
    closer.addEventListener("touchstart", (e) => {
      e.stopPropagation();
      closePopup();
    });

    pop.onkeydown = (event) => {
      if (event.key === "Escape") {
        unhighlighQuestion(qId, view.state, view.dispatch);
        closePopup();
      }
    };

    let editorHolder = pop.appendChild(document.createElement("div"));
    editorHolder.className = "questionEditorHolder";

    let bottomButtons = pop.appendChild(document.createElement("div"));

    if (view.editable) {
      const addAnswerButton = document.createElement("button");
      addAnswerButton.innerText = "+ Answer";
      addAnswerButton.className = "questionPopUpAddAnswer";

      addAnswerButton.onclick = (e) => {
        e.preventDefault();
        const qnode = newQuestionAnswerNode();
        const doc = qNode.editor.state.doc;
        const length = doc.content.size;
        const pos = length;
        const tr = qNode.editor.state.tr;
        tr.insert(pos, qnode);
        const sel = TextSelection.create(tr.doc, pos + 2);
        tr.setSelection(sel);

        qNode.editor.dispatch(tr);
        qNode.editor.focus();
      };

      bottomButtons.appendChild(addAnswerButton);
    }

    if (view.editable) {
      const trashButton = document.createElement("button");
      trashButton.innerText = "Delete";
      trashButton.className = "questionPopUpTrash";

      trashButton.onclick = (e) => {
        e.preventDefault();
        const questionId = qNode.node.attrs.questionId;
        removeQuestion(questionId, view.state, view.dispatch);
        closePopup();
      };

      bottomButtons.appendChild(trashButton);
    }

    let dispatchInner = (tr: Transaction) => {
      let { state, transactions } = qNode.editor.state.applyTransaction(tr);
      qNode.editor.updateState(state);

      if (!tr.getMeta("fromOutside")) {
        let outerTr = view.state.tr;
        let offsetMap = StepMap.offset(qNode.getPos() + 1);
        for (let i = 0; i < transactions.length; i++) {
          let steps = transactions[i].steps;
          for (let j = 0; j < steps.length; j++) outerTr.step(steps[j].map(offsetMap));
        }
        if (outerTr.docChanged) view.dispatch(outerTr);
      }
    };

    qNode.editor = new EditorView(editorHolder, {
      editable: () => view.editable,
      handleDOMEvents: {
        focus: () => {
          setCurrentEditor(qNode.editor);
        },
      },
      state: EditorState.create({
        schema: textSchema,
        doc: qNode.node,
        plugins: [
          history(),
          keymap({
            "Mod-z": undo,
            "Mod-y": redo,
            Tab: increaseLevel,
            "Mod-]": increaseLevel,
            "Shift-Tab": decreaseLevel,
            "Mod-[": decreaseLevel,
            "Mod-b": toggleBold,
            "Mod-i": toggleItalic,
            "Mod-u": toggleUnderline,
          }),
          keymap(baseKeymap),
        ],
      }),
      nodeViews: {
        questionAnswer(node) {
          return new QuestionAnswerView(node);
        },
      },
      dispatchTransaction: dispatchInner,
    });

    setTimeout(() => {
      qNode.editor.focus();
    }, 50);
  }
};

const questionMarkWidget = (
  qId: string,
  questionMap: Dictionary<QuestionMapItem>,
  setCurrentEditor: (view: EditorView) => void
) => (view: EditorView) => {
  // Use a real <button> so this control is keyboard-accessible
  const elem = document.createElement("button");
  elem.className = "questionMark";
  elem.setAttribute("aria-label", "Open question");
  const questionMarkImg = document.createElement("img");
  questionMarkImg.src = window.base + "/static/img/question-icon.svg";
  questionMarkImg.setAttribute("aria-hidden", "true");
  elem.appendChild(questionMarkImg);

  elem.addEventListener("mouseenter", (e) => {
    e.preventDefault();
    highlighQuestion(qId, view.state, view.dispatch);
  });

  elem.addEventListener("mouseleave", (e) => {
    e.preventDefault();
    let qNode = questionMap[qId];
    if (!qNode.editor) {
      unhighlighQuestion(qId, view.state, view.dispatch);
    }
  });

  elem.onmousedown = (e) => {
    e.preventDefault();
    e.stopPropagation();
    questionPopup(e.pageX, e.pageY, qId, questionMap, view, setCurrentEditor);
  };

  elem.addEventListener("keydown", (e) => {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      const r = elem.getBoundingClientRect();
      questionPopup(r.left + window.pageXOffset, r.bottom + window.pageYOffset, qId, questionMap, view, setCurrentEditor);
    }
  });

  return elem;
};

export const questionMarkPlugin = (
  questionMap: Dictionary<QuestionMapItem>,
  setCurrentEditor: (view: EditorView) => void
) =>
  new Plugin({
    props: {
      decorations(state: EditorState) {
        const decorations = [];
        const questions = {};
        state.doc.descendants((node, position) => {
          if (node.type.name === "section") return true;
          if (node.type.name === "bibleText") return true;
          if (node.type.name === "chunk") return true;
          if (node.type.name !== "text") return false;
          const questionRefs = node.marks.filter((m) => m.type.name === "questionReference");
          questionRefs.forEach((ref) => {
            const loc = position + node.nodeSize;
            const qId = ref.attrs.questionId;
            questions[qId] = loc;
          });
        });
        Object.keys(questions).forEach((qId) => {
          const loc = questions[qId];
          decorations.push(
            Decoration.widget(loc, questionMarkWidget(qId, questionMap, setCurrentEditor), {
              key: "qmark" + qId,
              stopEvent: (e: Event) => {
                return e.type === "click";
              },
              side: -1,
            })
          );
        });
        return DecorationSet.create(state.doc, decorations);
      },
    },
  });

export const addQuestion = (
  state: EditorState,
  dispatch: (tr: Transaction) => void,
  questionMap: Dictionary<QuestionMapItem>,
  view: EditorView,
  setCurrentEditor: (view: EditorView) => void
) => {
  const from = state.selection.from;
  const to = state.selection.to;

  if (dispatch) {
    const r = newQuestionNode();
    const qId = r[0];
    const qNode = r[1];

    let tr: Transaction = null;
    if (from !== to) {
      const qMark = textSchema.marks.questionReference.create({ questionId: qId });
      tr = state.tr.addMark(from, to, qMark);
    } else {
      tr = state.tr;
    }

    const sectionNode: Node = state.selection.$anchor.node(1);
    let posOfQuestions = null;
    let nodeQuestions = null;
    state.doc.descendants((node: Node, pos: number) => {
      if (node.eq(sectionNode)) {
        return true;
      }
      if (node.type.name === "studyBlocks") {
        return true;
      }
      if (node.type.name === "questions") {
        posOfQuestions = pos;
        nodeQuestions = node;
      }
      return false;
    });
    const qlength = nodeQuestions.content.size;
    const pos = posOfQuestions + qlength + 1;
    const tr1 = tr.insert(pos, qNode);
    dispatch(tr1);

    const coords = view.coordsAtPos(from);
    const popUpX = coords.left + window.pageXOffset;
    const popUpY = coords.bottom + window.pageYOffset;

    questionPopup(popUpX, popUpY, qId, questionMap, view, setCurrentEditor);

    return true;
  }
};

export function deleteQuestionSelection(
  state: EditorState,
  dispatch?: (tr: Transaction) => void
): boolean {
  const from = state.selection.from;
  const to = state.selection.to;
  if (to !== from) return false;
  if (dispatch) {
    const anchor = state.selection.$anchor;
    if (anchor.parentOffset !== 0) return false;
    const questionTextNode: Node = anchor.node(anchor.depth - 1);
    if (questionTextNode.type.name !== "questionText") return false;
    if (anchor.index(anchor.depth - 1) !== 0) return false;

    const questionNode: Node = anchor.node(anchor.depth - 2);
    return removeQuestion(questionNode.attrs.questionId, state, dispatch);
  }
}

export function deleteAnswerSelection(
  state: EditorState,
  dispatch?: (tr: Transaction) => void
): boolean {
  const from = state.selection.from;
  const to = state.selection.to;
  if (to !== from) return false;
  if (dispatch) {
    const anchor = state.selection.$anchor;
    if (anchor.parentOffset !== 0) return false;
    const answerTextNode: Node = anchor.node(anchor.depth - 1);
    if (answerTextNode.type.name !== "questionAnswer") return false;
    if (anchor.index(anchor.depth - 1) !== 0) return false;
    let pos = anchor.pos - 2;
    let endPos = pos + answerTextNode.nodeSize;
    const tr = state.tr.deleteRange(pos, endPos);

    dispatch(tr);
    return true;
  }
}

export function removeQuestion(
  questionId: string,
  state: EditorState,
  dispatch?: (tr: Transaction) => void
) {
  if (dispatch) {
    const qMark = textSchema.marks.questionReference.create({ questionId });
    const tr = state.tr.removeMark(0, state.doc.nodeSize - 2, qMark);

    let posOfQuestion = null;
    let nodeQuestion = null;
    tr.doc.descendants((node: Node, pos: number) => {
      if (node.type.name === "section") {
        return true;
      }
      if (node.type.name === "studyBlocks") {
        return true;
      }
      if (node.type.name === "questions") {
        return true;
      }
      if (node.type.name === "question") {
        if (node.attrs.questionId === questionId) {
          posOfQuestion = pos;
          nodeQuestion = node;
        }
        return false;
      }
      return false;
    });
    if (posOfQuestion !== null) {
      tr.deleteRange(posOfQuestion, posOfQuestion + nodeQuestion.nodeSize);
    }

    dispatch(tr);
    return true;
  }
}
