import {
  EditorState,
  Plugin,
  Transaction,
  TextSelection,
} from "prosemirror-state";
import { EditorView, NodeView, DecorationSet, Decoration } from "prosemirror-view";
import { Node, Mark } from "prosemirror-model";
import { StepMap } from "prosemirror-transform";
import { history } from "prosemirror-history";
import { keymap } from "prosemirror-keymap";
import { baseKeymap } from "prosemirror-commands";
import { undo, redo } from "prosemirror-history";
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
      // Logic to move the cursor to the previous position
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
      noQuestionsText.innerHTML = `<em>Insert a question by selecting some text and clicking the "Add Question" button</em> <img src="${window.base}/static/img/question-icon.svg" alt="Add Question Icon"> <em>in the toolbar above</em>`;

      noQuestionsText.onclick = () => {
        // Logic to move the cursor to the previous position
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

    if (!questionMap[this.questionId]) {
      this.questionMap = questionMap;
      questionMap[this.questionId] = {
        node: node,
        getPos: getPos,
        editor: null,
      };
    }

    this.dom = document.createElement("questionOuter");
    const qtext = document.createElement("div");
    qtext.setAttribute("contenteditable", "false");
    qtext.innerText = formatBibleReference(vStart, vEnd);
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
  let qselector = 'span[data-type="reference"][referenceId="' + rId + '"]';
  mview.onmouseenter = (e) => {
    // e.preventDefault();
    // var references = document.querySelectorAll(qselector);
    // references.forEach( (r) => {
    //   r.classList.add("referenceTo");
    // })
  };
  mview.onmouseleave = (e) => {
    e.preventDefault();
    // var references = document.querySelectorAll(qselector);
    // references.forEach( (r) => {
    //   r.classList.remove("referenceTo");
    // })
  };
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

    // Initially position it off-screen so it doesn't flicker
    pop.style.position = "absolute";
    pop.style.left = "-9999px";
    pop.style.top = "-9999px";

    // Add it to the body so it renders and we can measure it
    const p215EditorHolder = document.getElementById("editorHolder");
    p215EditorHolder.appendChild(pop);

    // Now measure it
    const rect = pop.getBoundingClientRect();
    const windowWidth = window.innerWidth;
    const windowHeight = document.body.scrollHeight;

    // Calculate initial position based on incoming x, y
    let initialLeft = x - 20;
    let initialTop = y + parseInt(getComputedStyle(document.documentElement).fontSize);

    // Check if it would appear off-screen and adjust
    if (initialLeft + rect.width > windowWidth) {
      initialLeft = windowWidth - rect.width;
    }
    if (initialTop + rect.height > windowHeight) {
      initialTop = windowHeight - rect.height;
    }

    // Now position it correctly and make it visible
    pop.style.left = initialLeft + "px";
    pop.style.top = initialTop + "px";
    pop.style.visibility = "visible";

    // Increase the z-index of the current pop-up
    if (!zIndices[qId]) {
      zIndices[qId] = zIndexCounter++;
    }
    pop.style.zIndex = zIndices[qId].toString();

    // Set a higher z-index for the focused pop-up, lower for others
    pop.addEventListener("mousedown", () => {
      const currentZIndex = zIndices[qId];
      const highestZIndex = Math.max(...Object.values(zIndices));
      if (currentZIndex < highestZIndex) {
        zIndices[qId] = highestZIndex + 1;
        pop.style.zIndex = zIndices[qId].toString();
      }
    });

    // Add ref highlight class
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
        const windowHeight = document.body.scrollHeight;
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

    // Resize handle
    const resizeHandle = pop.appendChild(document.createElement("div"));
    resizeHandle.className = "resizeHandle";

    // Edge highlight overlay (no pointer events — purely visual)
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
        if (edges.right) pop.style.width = Math.max(200, startW + dx) + "px";
        if (edges.bottom) pop.style.height = Math.max(150, startH + dy) + "px";
        if (edges.left) {
          const newW = Math.max(200, startW - dx);
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

    let closer = mover.appendChild(document.createElement("closer"));
    let closeImage = document.createElement("img");
    closer.className = "closer";
    closeImage.src = window.base + "/static/img/x.svg";
    closer.appendChild(closeImage);
    closer.onclick = (e) => {
      unhighlighQuestion(qId, view.state, view.dispatch);
      qNode.editor.destroy();
      qNode.editor = null;
      pop.parentNode.removeChild(pop);
    };
    closer.addEventListener("touchstart", (e) => {
      e.stopPropagation();
      qNode.editor.destroy();
      qNode.editor = null;
      pop.parentNode.removeChild(pop);
    });

    pop.onkeydown = (event) => {
      if (event.key === "Escape") {
        unhighlighQuestion(qId, view.state, view.dispatch);
        qNode.editor.destroy();
        qNode.editor = null;
        pop.parentNode.removeChild(pop);
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

        qNode.editor.destroy();
        qNode.editor = null;
        pop.parentNode.removeChild(pop);
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
  const elem = document.createElement("div");
  elem.className = "questionMark";
  elem.innerHTML = `<img src='${window.base}/static/img/question-icon.svg'/>`;

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
