import {
  toggleBold,
  toggleItalic,
  toggleUnderline,
  clearFormatting,
  setTextColor,
  setHighlightColor,
  removeHighlightColor,
  getCurrentTextAndHighlightColors,
} from "./editorUtils";

import {
  EditorState,
  Plugin,
  PluginKey,
  Transaction,
  EditorStateConfig,
  Selection,
  TextSelection,
} from "prosemirror-state";
import { EditorView, NodeView, DecorationSet, Decoration } from "prosemirror-view";
import { undoItem, redoItem, MenuItem, MenuItemSpec, menuBar } from "prosemirror-menu";
import { textSchema } from "./textSchema";
import {
  chainCommands,
  deleteSelection,
  joinBackward,
  selectNodeBackward,
  toggleMark,
} from "prosemirror-commands";

import { undo, redo, history } from "prosemirror-history";
import { keymap } from "prosemirror-keymap";
import { Slice, Node, Mark, Fragment } from "prosemirror-model";
import { StepMap, Step, Transform } from "prosemirror-transform";
import { baseKeymap } from "prosemirror-commands";
import "./styles.css";
import * as classes from "./styles.module.scss";
import QuestionIcon from "../Pages/Study/TextEditorToolbar/Assets/question-icon.svg";
import AddScriptureIcon from "../Assets/add-scripture.svg";
import {
  questionHighlightPlugin,
  highlighQuestion,
  unhighlighQuestion,
} from "./QuestionHighlightPlugin";

import CloseXIcon from "../Assets/x.svg";

// var blockMap : Dictionary<BlockMapItem> = {};

class SectionView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  constructor(node: Node, sectionIndex: number) {
    this.dom = document.createElement("div");
    this.dom.className = classes.section;
    // this.dom.id = `section-${sectionIndex}`;
    this.contentDOM = document.createElement("div");
    this.contentDOM.className = classes.content;
    this.dom.appendChild(this.contentDOM);
  }
}

class StudyBlocksView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  constructor(node: Node) {
    this.dom = document.createElement("table");
    this.dom.className = classes.studyBlocks;
    this.contentDOM = this.dom;
  }
}

function getRandomStr(): string {
  const arrayb = new Uint8Array(10);
  let b = self.crypto.getRandomValues(arrayb);
  return btoa(b.reduce((a, b) => a + b, ""));
}

function newQuestionNode(): [string, Node] {
  const questionId = getRandomStr();
  const p = textSchema.nodes.paragraph.create();
  const questionText = textSchema.nodes.questionText.create({}, p);
  const result = textSchema.nodes.question.create({ questionId }, questionText);
  return [questionId, result];
}

const newQuestionAnswerNode = () => {
  const p = textSchema.nodes.paragraph.create();
  const questionAnswer = textSchema.nodes.questionAnswer.create({}, p);
  return questionAnswer;
};

class QuestionsView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  node: Node;
  questionCount: number;
  constructor(node: Node, view: EditorView, getPos: () => number) {
    this.node = node;
    this.dom = document.createElement("tr");
    this.dom.className = classes.questions;

    const header = document.createElement("td");
    header.setAttribute("contenteditable", "false");

    const headerDiv = document.createElement("div");
    headerDiv.setAttribute("contenteditable", "false");
    headerDiv.innerText = "Questions";
    headerDiv.className = classes.studyBlockHeaderDiv;

    header.appendChild(headerDiv);
    this.dom.appendChild(header);

    this.contentDOM = document.createElement("td");

    if (node.content.size === 0) {
      const noQuestionsText = document.createElement("div");
      noQuestionsText.className = classes.noQuestionsText;
      noQuestionsText.innerHTML = `<em>Questions appear in this section. Insert a question by clicking the "Add Question" button</em> <img src="${QuestionIcon}" alt="Add Question Icon"> <em>in the toolbar above</em>`;
      this.contentDOM.appendChild(noQuestionsText);
    }

    this.dom.appendChild(this.contentDOM);
  }
  update(node: Node) {
    this.node = node;
    return true;
  }
}

class GeneralStudyBlockHeader implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  node: Node;
  constructor(node: Node) {
    this.node = node;
    this.dom = document.createElement("td");
    this.contentDOM = document.createElement("div");
    this.contentDOM.className = classes.studyBlockHeaderDiv;
    this.dom.appendChild(this.contentDOM);
  }
  update(node: Node) {
    this.node = node;
    return true;
  }
}

type BibleVerse = {
  book: string;
  chapter: number;
  verse: number;
};

function formatBibleReference(verse1: BibleVerse, verse2: BibleVerse): string {
  // Check if the books are different
  if (verse1 == null || verse2 == null) return "Q";
  if (verse1.book !== verse2.book) {
    return "Q";
  }

  // Check if both the chapter and verse are the same
  if (verse1.chapter === verse2.chapter && verse1.verse === verse2.verse) {
    return `${verse1.chapter}:${verse1.verse}`;
  }

  // Check if only the chapters are the same
  if (verse1.chapter === verse2.chapter) {
    return `${verse1.chapter}:${verse1.verse}-${verse2.verse}`;
  }

  // If chapters are different
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
      addAnswer.className = classes.addAnswer;
      this.dom.appendChild(addAnswer);
    }

    // const deleteQuestion = document.createElement("button");
    // deleteQuestion.onclick = (e) => {
    //   e.preventDefault();
    //   removeQuestion(this.questionId, view.state, view.dispatch);
    //   delete this.questionMap[this.questionId];
    // };
    // deleteQuestion.innerText = "delete question";
    // deleteQuestion.className = classes.deleteQuestion;
    // this.dom.appendChild(deleteQuestion);
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

export class ChunkView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  button: HTMLElement;

  constructor(node: Node, view: EditorView, getPos: () => number) {
    this.dom = document.createElement("p");
    this.dom.className = classes.outerChunk;
    this.contentDOM = document.createElement("p");
    this.contentDOM.className = classes.chunk;
    this.contentDOM.setAttribute("level", node.attrs.level);
    this.dom.appendChild(this.contentDOM);

    // this.dom.onmouseenter = (e) => {
    //   let hasChunkComment = this.hasComment(node);

    //   if (hasChunkComment) return;

    //   e.preventDefault();
    //   let widget = Decoration.widget(getPos()+1, () => {
    //     const but = document.createElement("div");
    //     but.className = classes.chunkButton;
    //     but.onclick = () => {
    //       const chunkCom = textSchema.nodes.chunkComment.create({
    //         content: Fragment.empty,
    //       });
    //       view.dispatch(view.state.tr.insert(getPos()+1, chunkCom));
    //     };
    //     return but;
    //   });
    //   view.setProps({
    //     decorations: (state) => {
    //       return DecorationSet.create(state.doc, [
    //         widget
    //       ]);
    //     },
    //   });
    // }
  }

  hasComment(node: Node) {
    let hasChunkComment = false;
    node.descendants((node: Node) => {
      if (node.type.name == "chunkComment") hasChunkComment = true;
      return false;
    });
    return hasChunkComment;
  }
}

export class ChunkCommentView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  popup: HTMLElement;
  node: Node;
  outerView: EditorView;
  innerView: EditorView;
  getPos: () => number;

  constructor(node: Node, view: EditorView, getPos: () => number) {
    this.getPos = getPos;
    this.outerView = view;
    this.node = node;
    this.dom = document.createElement("chunkComment");
    this.dom.className = classes.chunkButton;
    this.popup = null;
    this.dom.onclick = (e) => {
      if (!this.popup) this.open();
    };
  }

  close() {
    if (this.popup) {
      this.innerView.destroy();
      this.innerView = null;
      this.dom.textContent = "";
      // I don't know why I have to do this. Probably because of some magic in
      // prosemirror.
      setTimeout(() => {
        this.popup = null;
      }, 0);
    }
  }

  update(node: Node) {
    // if (!node.sameMarkup(this.node)) return false
    this.node = node;
    if (this.innerView) {
      let state = this.innerView.state;
      let start = node.content.findDiffStart(state.doc.content);
      if (start != null) {
        let { a: endA, b: endB } = node.content.findDiffEnd(state.doc.content);
        let overlap = start - Math.min(endA, endB);
        if (overlap > 0) {
          endA += overlap;
          endB += overlap;
        }
        this.innerView.dispatch(
          state.tr.replace(start, endB, node.slice(start, endA)).setMeta("fromOutside", true)
        );
      }
    }
    return true;
  }

  open() {
    let pop = this.dom.appendChild(document.createElement("div"));
    this.popup = pop;
    pop.className = classes.chunkCommentPopup;

    let closeBut = pop.appendChild(document.createElement("button"));
    closeBut.innerText = "Close";
    closeBut.onclick = (e) => {
      e.preventDefault();
      this.close();
    };

    let addRefBut = pop.appendChild(document.createElement("button"));
    addRefBut.innerText = "Add Reference";
    addRefBut.onclick = (e) => {
      e.preventDefault();
      // const rId = crypto.randomUUID();
      // const meta = {
      //   addedReference: true,
      //   currentSelection:null,
      //   referenceId: rId,
      // };
      // const tr =
      //   this.outerView.state.tr
      //     .setNodeAttribute(this.getPos(), "referenceId", rId)
      //     .setMeta(referencePluginKey, meta);
      // this.outerView.dispatch(tr);
    };

    let editorHolder = pop.appendChild(document.createElement("div"));
    this.innerView = new EditorView(editorHolder, {
      state: EditorState.create({
        doc: this.node,
      }),
      dispatchTransaction: this.dispatchInner.bind(this),
    });
  }

  dispatchInner(tr: Transaction) {
    let { state, transactions } = this.innerView.state.applyTransaction(tr);
    this.innerView.updateState(state);

    if (!tr.getMeta("fromOutside")) {
      let outerTr = this.outerView.state.tr,
        offsetMap = StepMap.offset(this.getPos() + 1);
      for (let i = 0; i < transactions.length; i++) {
        let steps = transactions[i].steps;
        for (let j = 0; j < steps.length; j++) outerTr.step(steps[j].map(offsetMap));
      }
      if (outerTr.docChanged) this.outerView.dispatch(outerTr);
    }
  }
}

class CustomEditorView extends EditorView {
  constructor(place, props) {
    super(place, props);
  }

  toggleBold() {
    if (this.editable) {
      toggleBold(this.state, this.dispatch);
      this.focus();
    }
  }

  toggleItalic() {
    if (this.editable) {
      toggleItalic(this.state, this.dispatch);
      this.focus();
    }
  }

  toggleUnderline() {
    if (this.editable) {
      toggleUnderline(this.state, this.dispatch);
      this.focus();
    }
  }

  clearFormatting() {
    if (this.editable) {
      clearFormatting(this.state, this.dispatch);
      this.focus();
    }
  }

  setTextColor(color: string) {
    if (this.editable) {
      setTextColor(this.state, this.dispatch, color);
      this.focus();
    }
  }

  setHighlightColor(color: string) {
    if (this.editable) {
      setHighlightColor(this.state, this.dispatch, color);
      this.focus();
    }
  }

  removeHighlightColor() {
    if (this.editable) {
      removeHighlightColor(this.state, this.dispatch);
      this.focus();
    }
  }

  getCurrentTextAndHighlightColors(setHighlightFillColor, setTextFillColor) {
    getCurrentTextAndHighlightColors(this.state, setHighlightFillColor, setTextFillColor);
    this.focus();
  }

  increaseLevel() {
    if (this.editable) {
      increaseLevel(this.state, this.dispatch);
      this.focus();
    }
  }

  decreaseLevel() {
    if (this.editable) {
      decreaseLevel(this.state, this.dispatch);
      this.focus();
    }
  }
}

const zIndices: { [key: string]: number } = {}; // Object to store z-index values for pop-ups
let zIndexCounter: number = 17; // Start z-index from 17

const questionPopup = (
  x,
  y,
  qId,
  questionMap: Dictionary<QuestionMapItem>,
  view: EditorView,
  setActiveEditor
) => {
  let qNode = questionMap[qId];
  if (!qNode.editor) {
    const pop = document.createElement("questionRefPopup");
    pop.className = classes.questionRefPopup;

    // Initially position it off-screen so it doesn't flicker
    pop.style.position = "absolute";
    pop.style.left = "-9999px";
    pop.style.top = "-9999px";

    // Add it to the body so it renders and we can measure it
    document.body.appendChild(pop);

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
        // Increase the z-index only if it's not the highest
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

        // Boundary checks
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
      startDrag(e.clientX, e.clientY);
    });

    mover.addEventListener("touchstart", (e) => {
      e.preventDefault();
      const touch = e.touches[0];
      startDrag(touch.clientX, touch.clientY);
    });

    let popUpTitle = mover.appendChild(document.createElement("p"));
    popUpTitle.innerHTML = "Question";
    popUpTitle.className = classes.QpopUpTitle;

    //add question icon
    let questionIconImg = document.createElement("img");
    questionIconImg.src = QuestionIcon;

    popUpTitle.prepend(questionIconImg);

    // Add close Icon
    let closer = mover.appendChild(document.createElement("closer"));
    let closeImage = document.createElement("img");
    closer.className = classes.closer;
    closeImage.src = CloseXIcon;
    closer.appendChild(closeImage);
    closer.onclick = (e) => {
      //turn off ref highlight
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
        //turn off ref highlight
        unhighlighQuestion(qId, view.state, view.dispatch);

        qNode.editor.destroy();
        qNode.editor = null;
        pop.parentNode.removeChild(pop);
      }
    };

    let editorHolder = pop.appendChild(document.createElement("div"));
    editorHolder.className = classes.questionEditorHolder;

    let bottomButtons = pop.appendChild(document.createElement("div"));

    //Add "add answer" button
    if (view.editable) {
      const addAnswerButton = document.createElement("button");
      addAnswerButton.innerText = "+ Answer";
      addAnswerButton.className = classes.questionPopUpAddAnswer;

      addAnswerButton.onclick = (e) => {
        e.preventDefault();
        const qnode = newQuestionAnswerNode();
        const doc = qNode.editor.state.doc;
        const length = doc.content.size;
        const pos = length;
        const tr = qNode.editor.state.tr;
        tr.insert(pos, qnode);
        const sel = TextSelection.create(tr.doc, pos + 3);
        tr.setSelection(sel);

        qNode.editor.dispatch(tr);
      };

      bottomButtons.appendChild(addAnswerButton);
    }

    //Add "add trash" button
    if (view.editable) {
      const trashButton = document.createElement("button");
      trashButton.innerText = "Delete";
      trashButton.className = classes.questionPopUpTrash;

      trashButton.onclick = (e) => {
        e.preventDefault();
        // Delete the current question
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

    qNode.editor = new CustomEditorView(editorHolder, {
      editable: () => view.editable,
      handleDOMEvents: {
        focus: () => {
          setActiveEditor(qNode.editor);
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
            "Mod-b": toggleMark(textSchema.marks.strong),
            "Mod-i": toggleMark(textSchema.marks.em),
            "Mod-u": toggleMark(textSchema.marks.underline),
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

    // Automatically focus the pop-up's editable area
    setTimeout(() => {
      qNode.editor.focus();
    }, 50);
  }
};

export const questionReferenceMarkView = (mark: Mark, view: EditorView) => {
  const mview = document.createElement("questionRef");
  mview.className = classes.questionRef;
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
    //   r.classList.add(classes.referenceTo);
    // })
  };
  mview.onmouseleave = (e) => {
    e.preventDefault();
    // var references = document.querySelectorAll(qselector);
    // references.forEach( (r) => {
    //   r.classList.remove(classes.referenceTo);
    // })
  };
  return { dom: mview };
};

const nodeIsChunk = (node: Node) => {
  if (node.type.name === "section") return true;
  if (node.type.name === "bibleText") return true;
  if (node.type.name != "chunk") return false;
};

let currentChunkPlug = new Plugin({
  props: {
    decorations(state: EditorState) {
      const selection = state.selection;
      const decorations = [];

      if (!selection.empty) return null;

      state.doc.nodesBetween(selection.from, selection.to, (node, position) => {
        const result = nodeIsChunk(node);
        if (result === true) return true;
        if (result === false) return false;
        decorations.push(
          Decoration.node(position, position + node.nodeSize, { class: classes.selected })
        );
      });

      return DecorationSet.create(state.doc, decorations);
    },
  },
});

const questionMarkWidget =
  (qId: string, questionMap: Dictionary<QuestionMapItem>, setActiveEditor) =>
  (view: EditorView) => {
    const elem = document.createElement("div");
    elem.className = classes.questionMark;
    elem.innerHTML = '<img src="' + QuestionIcon + '" />';

    // Add mouseenter event listener to add a class to questionRef
    elem.addEventListener("mouseenter", (e) => {
      e.preventDefault();
      highlighQuestion(qId, view.state, view.dispatch);
    });

    // Add mouseleave event listener to remove the class from questionRef
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
      questionPopup(e.pageX, e.pageY, qId, questionMap, view, setActiveEditor);
    };
    return elem;
  };

let questionMarkPlugin = (questionMap: Dictionary<QuestionMapItem>, setActiveEditor) =>
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
            Decoration.widget(loc, questionMarkWidget(qId, questionMap, setActiveEditor), {
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

const verseRefWidget = (verse, position) => (view: EditorView) => {
  const elem = document.createElement("span");
  elem.onclick = (e) => {
    e.preventDefault();
    const transaction = view.state.tr.setSelection(TextSelection.create(view.state.doc, position));
    view.dispatch(transaction);
    view.focus();
  };
  elem.ondblclick = (e) => {
    e.preventDefault();
    let book = verse.book.replace(" ", "_").toLowerCase();
    let url = "https://biblehub.com/" + book + "/" + verse.chapter + "-" + verse.verse + ".htm";
    window.open(url, "_blank").focus();
    view.focus();
  };
  elem.className = classes.verseRef;
  elem.contentEditable = "true";
  if (verse.verse === 1) {
    elem.innerHTML = verse.chapter + ":" + verse.verse;
  } else {
    elem.innerHTML = verse.verse;
  }
  return elem;
};

function getDecorations(state: EditorState) {
  const decorations = [];
  let lastVerseKey = null;
  let sectionIndex = -1;

  state.doc.descendants((node, position) => {
    if (node.type.name === "section") {
      sectionIndex++;
      return true;
    }
    if (node.type.name === "bibleText") return true;
    if (node.type.name === "chunk") return true;
    if (node.type.name !== "text") return false;

    const verse = node.marks.find((m) => m.type.name === "verse");
    if (!verse) return true;

    // Construct a unique key for each verse
    const currentVerseKey =
      sectionIndex + "-" + verse.attrs.book + "-" + verse.attrs.chapter + "-" + verse.attrs.verse;

    // Check if we encountered a different verse
    if (lastVerseKey !== currentVerseKey) {
      lastVerseKey = currentVerseKey; // Update the last verse key

      // Create decoration for the first occurrence of this verse
      decorations.push(
        Decoration.widget(position, verseRefWidget(verse.attrs, position), {
          key: currentVerseKey,
          ignoreSelection: true,
          side: -1,
        })
      );
    }

    return false;
  });

  return decorations;
}

let verseReferencePlugin = new Plugin({
  props: {
    decorations(state: EditorState) {
      return DecorationSet.create(state.doc, getDecorations(state));
    },
    handleKeyDown(view, event) {
      if (event.key === "ArrowUp") {
        return false; // Return false to indicate that this handler has not handled the event
      } else if (event.key === "ArrowLeft") {
        // left arrow key
        const { state, dispatch } = view;
        const { selection } = state;
        const decorations = getDecorations(state);
        const widgetAtSelection = DecorationSet.create(state.doc, decorations).find(
          selection.from,
          selection.from
        );
        if (widgetAtSelection.length) {
          event.preventDefault();
          dispatch(state.tr.setSelection(TextSelection.create(state.tr.doc, selection.from - 1)));
          return true;
        }
      }
      return false;
    },
  },
});

let sectionIdPlugin = new Plugin({
  props: {
    decorations(state: EditorState) {
      const decorations = [];

      let index = 0;
      state.doc.descendants((node, position) => {
        if (node.type.name == "section") {
          decorations.push(
            Decoration.node(position, position + node.nodeSize, { id: `section-${index}` })
          );
          index++;
        }
        return false;
      });
      return DecorationSet.create(state.doc, decorations);
    },
  },
});

const increaseLevel = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
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
  if (dispatch)
    dispatch(
      toTransform.reduce((pre, { pos, node }) => {
        return pre.setNodeMarkup(pos, type, { level: node.attrs.level + 1 }, null);
      }, state.tr)
    );
  return true;
};

const decreaseLevel = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
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

const addQuestion = (
  state: EditorState,
  dispatch: (tr: Transaction) => void,
  questionMap: Dictionary<QuestionMapItem>,
  view: EditorView,
  setActiveEditor
) => {
  const from = state.selection.from;
  const to = state.selection.to;

  // Function to extract verse info from a mark
  const extractVerseInfo = (verseMark) => {
    return {
      book: verseMark.attrs.book,
      chapter: verseMark.attrs.chapter,
      verse: verseMark.attrs.verse,
    };
  };

  if (dispatch) {
    const r = newQuestionNode();
    const qId = r[0];
    const qNode = r[1];

    // Make the mark
    let tr: Transaction = null;
    if (from !== to) {
      const qMark = textSchema.marks.questionReference.create({ questionId: qId });
      tr = state.tr.addMark(from, to, qMark);
    } else {
      tr = state.tr;
    }

    // Find the position of the questions
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

    // Calculate position for the pop-up based on the cursor's current position
    const coords = view.coordsAtPos(from);
    const popUpX = coords.left + window.pageXOffset; // X coordinate
    const popUpY = coords.bottom + window.pageYOffset; // Y coordinate

    questionPopup(popUpX, popUpY, qId, questionMap, view, setActiveEditor);

    return true;
  }
};

function deleteQuestionSelection(
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

function deleteAnswerSelection(state: EditorState, dispatch?: (tr: Transaction) => void): boolean {
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

function removeQuestion(
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

function moveSection(
  originalIndex: number,
  newIndex: number,
  state: EditorState,
  dispatch?: (tr: Transaction) => void
) {
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

let addVerse = (
  verseRef: string,
  passage: Array<AddVerse>,
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
          posOfStudyBlock = sectionPos + 1 + offset;
        }
        if (childNode.type.name === "sectionHeader") {
          posOfSectionHeader = sectionPos + 1 + offset;
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
        headerDiff = newHeader.nodeSize - sectionHeaderSize;
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

const addSection = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const from = state.selection.from;
  const to = state.selection.to;
  if (dispatch) {
    const node = newSectionNode();
    // should be the end of the document.
    const pos = state.doc.nodeSize - 2;
    const tr = state.tr.insert(pos, node);
    dispatch(tr);
    return true;
  }
};

const addGeneralStudyBlock = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
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

    // Create the study block and insert it at the found position
    const studyBlock = textSchema.nodes.generalStudyBlock.createChecked(null, [sbHeader, sbBody]);
    const tr = state.tr.insert(position, studyBlock);

    // Dispatch the transaction
    dispatch(tr);
    return true;
  }
};

const complexNodeTypes = new Set(["bibleText", "sectionHeader", "studyBlocks"]);

const preventUpdatingMultipleComplexNodesSelectionPlugin = new Plugin({
  key: new PluginKey("preventUpdatingMultipleComplexNodesSelection"),
  props: {
    handleDOMEvents: {
      keydown(view, event) {
        const { selection, doc } = view.state;
        let spansComplexNodes = false;
        let lastNodeName = null;
        let nodesEncountered = new Set();

        doc.nodesBetween(selection.from, selection.to, (node) => {
          if (complexNodeTypes.has(node.type.name)) {
            nodesEncountered.add(node.type.name);

            if (lastNodeName && lastNodeName !== node.type.name) {
              spansComplexNodes = true;
            }
            lastNodeName = node.type.name;
          }
        });

        if (nodesEncountered.size > 1 || spansComplexNodes) {
          event.preventDefault();
          return true;
        }

        if (event.keyCode === 8) {
          deleteSelection(view.state, view.dispatch);
        }

        return false;
      },
    },
  },
});
const mkPlaceholderElement = (pos) => (view: EditorView) => {
  const placeholderElement = document.createElement("div");
  placeholderElement.className = classes.bibleTextPlaceholder;
  placeholderElement.innerHTML = `<em>Place your cursor on this section's title and click the "Add Scripture" button</em> <img src="${AddScriptureIcon}" alt="Add Scripture Icon"> <em>above to add your verses</em>`;

  // Add an event listener to the placeholder
  placeholderElement.onclick = () => {
    console.log("hi");
    // Logic to move the cursor to the previous position
    const transaction = view.state.tr.setSelection(TextSelection.near(view.state.doc.resolve(pos)));
    view.dispatch(transaction);
    view.focus();
  };
  return placeholderElement;
};

function createPlaceholderDecorations(doc) {
  const decorations = [];

  doc.descendants((node, pos) => {
    if (node.type.name === "section") {
      const regex = /section\(sectionHeader\("([^"]+)"\)/g;
      let match;
      while ((match = regex.exec(node)) !== null) {
        const contentBetweenQuotes = match[1];

        const hasBibleText = node.content.content.some((child) => child.type.name === "bibleText");

        if (!hasBibleText) {
          const placeholderDecoration = Decoration.widget(
            pos + contentBetweenQuotes.length + 3,
            mkPlaceholderElement(pos)
          );
          decorations.push(placeholderDecoration);
        }
      }
    }
  });

  return decorations;
}

const bibleTextPlaceholderPlugin = new Plugin({
  state: {
    init(_, { doc }) {
      return DecorationSet.create(doc, createPlaceholderDecorations(doc));
    },
    apply(tr, oldState) {
      if (tr.docChanged) {
        return DecorationSet.create(tr.doc, createPlaceholderDecorations(tr.doc));
      }
      return oldState;
    },
  },
  props: {
    decorations(state) {
      return this.getState(state);
    },
  },
});

interface QuestionMapItem {
  node: Node;
  getPos: () => number;
  editor: EditorView | null;
}

type notUndefined = string | number | boolean | symbol | object;

interface Dictionary<T extends notUndefined = notUndefined> {
  [key: string]: T | undefined;
}

type RecieveFunc = (steps: any) => void;

type RemoteThingy = {
  setReceive: (fun: RecieveFunc) => void;
  send(steps: any);
};

export class P215Editor {
  state: EditorState;
  editable: boolean;
  view: EditorView;
  questionMap: Dictionary<QuestionMapItem>;
  updateHanlders: Array<(change: any) => void>;
  activeEditor: any;
  setActiveEditor: any;
  remoteThings: RemoteThingy;

  constructor({ initDoc, editable, activeEditor, setActiveEditor, remoteThings }) {
    this.editable = editable;
    this.remoteThings = remoteThings;
    let node = Node.fromJSON(textSchema, initDoc);
    this.updateHanlders = [];
    this.questionMap = {};

    this.activeEditor = activeEditor;
    this.setActiveEditor = setActiveEditor;

    baseKeymap["Backspace"] = chainCommands(
      deleteQuestionSelection,
      deleteAnswerSelection,
      joinBackward,
      selectNodeBackward
    );

    this.state = EditorState.create({
      schema: textSchema,
      doc: node,
      plugins: [
        history(),
        keymap({
          "Mod-z": undo,
          "Mod-y": redo,
          Tab: increaseLevel,
          "Mod-]": increaseLevel,
          "Shift-Tab": decreaseLevel,
          "Mod-[": decreaseLevel,
          "Mod-e": this.addQuestionCommand,
          "Mod-s": addGeneralStudyBlock,
          "Mod-b": toggleMark(textSchema.marks.strong),
          "Mod-i": toggleMark(textSchema.marks.em),
          "Mod-u": toggleMark(textSchema.marks.underline),
        }),
        keymap(baseKeymap),
        currentChunkPlug,
        questionMarkPlugin(this.questionMap, this.setActiveEditor),
        sectionIdPlugin,
        verseReferencePlugin,
        preventUpdatingMultipleComplexNodesSelectionPlugin,
        bibleTextPlaceholderPlugin,
        questionHighlightPlugin,
        // referencePlugin
      ],
    });
  }

  addQuestionCommand = (state, dispatch) => {
    return addQuestion(state, dispatch, this.questionMap, this.view, this.setActiveEditor);
  };

  addEditor(editorRoot: HTMLElement) {
    console.log(editorRoot);
    let that = this;
    this.view = new EditorView(editorRoot, {
      state: that.state,
      editable: () => that.editable,
      handlePaste: this.handlePaste.bind(this),
      handleDOMEvents: {
        focus: () => {
          this.setActiveEditor(this);
        },
      },
      nodeViews: {
        studyBlocks(node) {
          return new StudyBlocksView(node);
        },
        questions(node, view, getPos) {
          return new QuestionsView(node, view, getPos);
        },
        chunk(node, view, getPos) {
          return new ChunkView(node, view, getPos);
        },
        chunkComment(node, view, getPos) {
          return new ChunkCommentView(node, view, getPos);
        },
        question(node, view, getPos) {
          return new QuestionView(that.questionMap, node, view, getPos);
        },
        questionAnswer(node) {
          return new QuestionAnswerView(node);
        },
        generalStudyBlockHeader(node) {
          return new GeneralStudyBlockHeader(node);
        },
      },
      markViews: {
        referenceTo: referenceToMarkView,
        questionReference: questionReferenceMarkView,
      },
      dispatchTransaction: (transaction) => {
        let newState = that.view.state.apply(transaction);

        // Check if the last section is deleted
        if (newState.doc.childCount === 0) {
          const newSection = newSectionNode();
          const tr = newState.tr.insert(0, newSection);
          newState = newState.apply(tr);
        }

        that.view.updateState(newState);
        if (transaction.docChanged) {
          this.updateHanlders.forEach((handler) => {
            handler(transaction.doc.toJSON());
          });
          const steps = transaction.steps.map((st) => st.toJSON());

          if (this.remoteThings !== null && this.remoteThings.send) {
            this.remoteThings.send(steps);
          }
        }
      },
    });

    console.log(this.remoteThings);
    if (this.remoteThings != null && this.remoteThings.setReceive != null) {
      console.log("hello");
      this.remoteThings.setReceive((stepsRaw: any[]) => {
        const steps = stepsRaw.map((st) => Step.fromJSON(textSchema, st));
        const tr = this.view.state.tr;
        steps.forEach((st) => tr.step(st));
        this.view.dispatch(tr);
      });
    }
    console.log(this.remoteThings);
  }

  handlePaste(view, event, slice) {
    const { state, dispatch } = view;
    const { selection } = state;
    const targetNode = selection.$head.parent;

    // Handle paste into 'sectionHeader'
    if (targetNode.type.name === "sectionHeader") {
      event.preventDefault();

      // Extract text content from the slice and replace line breaks with spaces
      let textContent = "";
      slice.content.forEach((block) => {
        textContent += block.textContent.replace(/\n/g, " ") + " ";
      });

      // Insert the plain text into the editor
      dispatch(view.state.tr.insertText(textContent, selection.$head.pos, selection.$head.pos));

      return true;
    }

    // Below is the existing logic for handling other paste scenarios
    const allowedTextColor = new Set([
      "#000",
      "#91A4B1",
      "#8B4E35",
      "#F13A35",
      "#EB732E",
      "#F8AC2B",
      "#00A55B",
      "#2D78ED",
    ]);

    const allowedHighlightColor = new Set([
      "#EBDED9",
      "#FDDAD8",
      "#FCE6D6",
      "#FDF4D2",
      "#D2EFE0",
      "#C6E6FD",
      "#EFDEEC",
    ]);

    function processNode(node) {
      if (node.isText && node.marks.length) {
        // Adjust marks for text color and highlight color
        node.marks = node.marks
          .map((mark) => {
            if (mark.type.name === "textColor" && !allowedTextColor.has(mark.attrs.color)) {
              return null;
            } else if (
              mark.type.name === "highlightColor" &&
              !allowedHighlightColor.has(mark.attrs.color)
            ) {
              return null;
            }
            return mark;
          })
          .filter(Boolean); // Remove nulls from the array
      } else if (node.isBlock) {
        // Recursively process child nodes
        let newContent = [];
        node.content.forEach((childNode) => {
          newContent.push(processNode(childNode));
        });
        return node.copy(Fragment.from(newContent));
      }
      return node;
    }

    let newContent = [];
    slice.content.forEach((node) => {
      newContent.push(processNode(node));
    });

    // Process the slice and replace the original paste content
    let newSlice = new Slice(Fragment.from(newContent), slice.openStart, slice.openEnd);
    let transaction = view.state.tr.replaceSelection(newSlice);
    view.dispatch(transaction);

    return true;
  }

  removeEditor() {
    this.view.destroy();
  }

  addQuestion() {
    if (this.editable) {
      addQuestion(
        this.view.state,
        this.view.dispatch,
        this.questionMap,
        this.view,
        this.setActiveEditor
      );
      this.view.focus();
    }
  }

  increaseLevel() {
    if (this.editable) {
      increaseLevel(this.view.state, this.view.dispatch);
      this.view.focus();
    }
  }

  decreaseLevel() {
    if (this.editable) {
      decreaseLevel(this.view.state, this.view.dispatch);
      this.view.focus();
    }
  }

  undo() {
    if (this.editable) {
      const { state, dispatch } = this.view;
      undo(state, dispatch);
      this.view.focus();
    }
  }

  redo() {
    if (this.editable) {
      const { state, dispatch } = this.view;
      redo(state, dispatch);
      this.view.focus();
    }
  }

  toggleBold() {
    if (this.editable) {
      toggleBold(this.view.state, this.view.dispatch);
      this.view.focus();
    }
  }

  toggleItalic() {
    if (this.editable) {
      toggleItalic(this.view.state, this.view.dispatch);
      this.view.focus();
    }
  }

  toggleUnderline() {
    if (this.editable) {
      toggleUnderline(this.view.state, this.view.dispatch);
      this.view.focus();
    }
  }

  setTextColor(color: string) {
    if (this.editable) {
      setTextColor(this.view.state, this.view.dispatch, color);
      this.view.focus();
    }
  }

  setHighlightColor(color: string) {
    if (this.editable) {
      setHighlightColor(this.view.state, this.view.dispatch, color);
      this.view.focus();
    }
  }

  removeHighlightColor() {
    if (this.editable) {
      removeHighlightColor(this.view.state, this.view.dispatch);
      this.view.focus();
    }
  }

  getCurrentTextAndHighlightColors(setHighlightFillColor, setTextFillColor) {
    getCurrentTextAndHighlightColors(this.view.state, setHighlightFillColor, setTextFillColor);
  }

  clearFormatting() {
    if (this.editable) {
      clearFormatting(this.view.state, this.view.dispatch);
      this.view.focus();
    }
  }

  addSection() {
    if (this.editable) {
      addSection(this.view.state, this.view.dispatch);
    }
  }

  addVerse(verseRef: string, passage: Array<AddVerse>) {
    if (this.editable) {
      addVerse(verseRef, passage, this.view.state, this.view.dispatch);
      this.view.focus();
    }
  }

  insertLink(url: string, title: string = "") {
    if (this.editable) {
      const { state, dispatch } = this.view;
      const { tr, selection } = state;
      const link = state.schema.marks.link.create({ href: url, title });
      tr.addMark(selection.from, selection.to, link);
      dispatch(tr);
    }
  }

  moveSection(oldIndex: number, newIndex: number) {
    if (this.editable) {
      moveSection(oldIndex, newIndex, this.view.state, this.view.dispatch);
    }
  }

  deleteSection(sectionIndex: number) {
    if (this.editable) {
      let sectionPositions: number[] = [];
      this.view.state.doc.descendants((node, pos) => {
        if (node.type.name === "section") {
          sectionPositions.push(pos);
        }
      });

      if (sectionIndex >= sectionPositions.length) {
        console.error("Invalid section index");
        return;
      }

      const sectionPos = sectionPositions[sectionIndex];
      const sectionNode = this.view.state.doc.nodeAt(sectionPos);

      if (!sectionNode) {
        console.error("Couldn't find section node");
        return;
      }

      // Deleting the section
      const tr = this.view.state.tr.delete(sectionPos, sectionPos + sectionNode.nodeSize);
      this.view.dispatch(tr);
    }
  }

  addGeneralStudyBlock() {
    if (this.editable) {
      addGeneralStudyBlock(this.view.state, this.view.dispatch);
    }
  }

  onUpdate(f: (change: any) => void) {
    if (this.editable) {
      this.updateHanlders.push(f);
    }
  }
}
