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
import { toggleMark } from "prosemirror-commands";

import { undo, redo, history } from "prosemirror-history";
import { keymap } from "prosemirror-keymap";
import { Schema, Node, Mark, Fragment } from "prosemirror-model";
import { StepMap } from "prosemirror-transform";
import { baseKeymap } from "prosemirror-commands";
import "./styles.css";
import * as classes from "./styles.module.scss";

import DragHandleIcon from "../Assets/drag-handle.svg";
import CloseXIcon from "../Assets/x.svg";

let sectionCounter = 0;

const textSchema = new Schema({
  nodes: {
    doc: {
      content: "section*",
    },
    paragraph: {
      content: "text*",
      group: "richText",
      toDOM: () => {
        return ["p", 0];
      },
    },
    link: {
      attrs: {
        href: {},
        title: { default: null },
      },
      inclusive: false,
      parseDOM: [
        {
          tag: "a[href]",
          getAttrs(dom: any) {
            return {
              href: dom.getAttribute("href"),
              title: dom.getAttribute("title"),
            };
          },
        },
      ],
      toDOM(node: any) {
        const { href, title } = node.attrs;
        return ["a", { href, title }, 0];
      },
    },
    section: {
      content: "sectionChild*",
      isolating: true,
      defining: true,
    },
    sectionHeader: {
      content: "text*",
      group: "sectionChild",
      isolating: true,
      defining: true,
      toDOM: () => {
        return ["h2", 0];
      },
    },
    bibleText: {
      content: "chunk*",
      group: "sectionChild",
      isolating: true,
      defining: true,
      attrs: { bibRef: { default: "Genesis 1:1" },  },
      toDOM: () => {
        return [
          "div",
          {
            class: classes.bibleText,
          },
          0,
        ];
      },
    },
    studyBlocks: {
      content: "studyElement*",
      group: "sectionChild",
      isolating: true,
      defining: true,
    },
    questionDoc: {
      content: "question",
    },
    questions: {
      content: "question*",
      group: "studyElement",
      isolating: true,
      defining: true,
    },
    question: {
      content: "(questionAnswer | questionText )*",
      attrs: { questionId: { default: null } },
      isolating: true,
      defining: true,
      draggable: true,
      toDOM: () => {
        return ["question", { class: classes.question }, 0];
      },
    },
    generalStudyBlock: {
      content: "generalStudyBlockChildren*",
      group: "studyElement",
      isolating: true,
      defining: true,
      toDOM: () => {
        return ["tr", 0];
      },
    },
    generalStudyBlockHeader: {
      content: "text*",
      group: "generalStudyBlockChildren",
      isolating: true,
      defining: true,
      toDOM: () => {
        return ["td", 0];
      },
    },
    generalStudyBlockBody: {
      content: "richText*",
      group: "generalStudyBlockChildren",
      isolating: true,
      defining: true,
      toDOM: () => {
        return ["td", 0];
      },
    },
    questionText: {
      isolating: true,
      defining: true,
      content: "richText*",
      toDOM: () => {
        return ["questionText", 0];
      },
    },
    questionAnswer: {
      content: "richText*",
      draggable: true,
      toDOM: () => {
        return ["questionAnswer", 0];
      },
    },
    chunkComment: {
      content: "text*",
      inline: true,
      atom: true,
      draggable: false,
      defining: true,
      selectable: false,
      attrs: { referenceId: { default: null }, color: { default: "red" } },
      toDOM: () => ["chunkComment", 0],
      parseDOM: [{ tag: "chunkComment" }],
    },
    chunk: {
      content: "(chunkComment | text)*",
      attrs: { level: { default: 0 } },
      toDOM: (node) => {
        return [
          "div",
          {
            class: classes.chunk,
            level: node.attrs.level,
            style: "margin-left:" + 1.5 * node.attrs.level + "em;",
          },
          0,
        ];
      },
    },
    text: { inline: true },
  },
  marks: {
    strong: {
      parseDOM: [{ tag: "strong" }],
      toDOM: () => ["strong"],
    },
    em: {
      parseDOM: [{ tag: "em" }],
      toDOM: () => ["em"],
    },
    underline: {
      parseDOM: [{ tag: "u" }],
      toDOM: () => ["u"],
    },
    textColor: {
      attrs: { color: {} },
      inline: true,
      parseDOM: [
        {
          style: "color",
          getAttrs: (value) => {
            return { color: value };
          },
        },
      ],
      toDOM: (mark) => {
        return ["span", { style: `color: ${mark.attrs.color};` }, 0];
      },
    },
    highlightColor: {
      attrs: { color: {} },
      inline: true,
      parseDOM: [
        {
          style: "background-color",
          getAttrs: (value) => {
            return { color: value };
          },
        },
      ],
      toDOM: (mark) => {
        return ["span", { style: `background-color: ${mark.attrs.color};` }, 0];
      },
    },
    referenceTo: {
      attrs: { referenceId: {} },
      excludes: "",
      toDOM: (mark) => {
        return [
          "span",
          {
            "data-type": "reference",
            class: classes.reference,
            referenceId: mark.attrs.referenceId,
          },
          0,
        ];
      },
    },
    questionReference: {
      attrs: { questionId: {} },
      excludes: "",
      toDOM: (mark) => [
        "questionRef",
        {
          class: classes.questionRef,
          questionId: mark.attrs.questionId,
        },
        0,
      ],
    },
  },
});

// var blockMap : Dictionary<BlockMapItem> = {};

class SectionView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  constructor(node: Node, sectionIndex: number) {
    this.dom = document.createElement("div");
    this.dom.className = classes.section;
    this.dom.id = `section-${sectionIndex}`;
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

const newQuestionNode: () => [string, Node] = () => {
  const questionId = getRandomStr();
  const p = textSchema.nodes.paragraph.create();
  const questionText = textSchema.nodes.questionText.create({}, p);
  const result = textSchema.nodes.question.create({ questionId }, questionText);
  return [questionId, result];
};

const newQuestionAnswerNode = () => {
  const p = textSchema.nodes.paragraph.create();
  const questionAnswer = textSchema.nodes.questionAnswer.create({}, p);
  return questionAnswer;
};

class QuestionsView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  node: Node;
  constructor(node: Node, view: EditorView, getPos: () => number) {
    this.node = node;
    this.dom = document.createElement("tr");
    this.dom.className = classes.questions;
    const header = document.createElement("td");
    header.setAttribute("contenteditable", "false");
    header.className = classes.questionsLabel;
    header.innerText = "Questions";
    const addButton = document.createElement("button");
    addButton.innerHTML = "+ Question";
    addButton.onclick = (e) => {
      e.preventDefault();
      const qnode = newQuestionNode()[1];
      const length = this.node.content.size;
      const pos = getPos() + length;
      const tr1 = view.state.tr.insert(pos + 1, qnode);
      const sel = TextSelection.create(tr1.doc, pos + 4);
      const tr2 = tr1.setSelection(sel);
      view.dispatch(tr2);
    };
    header.appendChild(addButton);
    this.dom.appendChild(header);
    this.contentDOM = document.createElement("td");
    this.dom.appendChild(this.contentDOM);
  }
  update(node: Node) {
    this.node = node;
    return true;
  }
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
    qtext.innerText = "Q:";
    this.dom.appendChild(qtext);
    this.contentDOM = document.createElement("question");
    this.dom.appendChild(this.contentDOM);
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

    this.setPadding(node);

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

  setPadding(node: Node) {
    let style = "padding-left: calc(" + 2 * node.attrs.level + "em + 7px);";
    this.dom.setAttribute("style", style);
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

const questionPopup = (x, y, qId, questionMap: Dictionary<QuestionMapItem>, view: EditorView) => {
  let qNode = questionMap[qId];
  if (!qNode.editor) {
    const pop = document.createElement("questionRefPopup");
    pop.className = classes.questionRefPopup;
    document.body.appendChild(pop);
    pop.style.left = "calc(" + x + "px - 20px)";
    pop.style.top = "calc(" + y + "px + 1em)";
    let mover = pop.appendChild(document.createElement("mover"));
    mover.onmousedown = (e) => {
      e.stopPropagation();
      e.preventDefault();
      const rec = pop.getBoundingClientRect();
      const diffX = e.pageX - rec.x;
      const diffY = e.pageY - rec.y;
      const mousemove = (e: MouseEvent) => {
        e.stopPropagation();
        e.preventDefault();
        pop.style.left = "calc(" + (e.pageX - diffX) + "px)";
        pop.style.top = "calc(" + (e.pageY - diffY) + "px)";
      };
      document.addEventListener("mouseup", (e) => {
        document.removeEventListener("mousemove", mousemove);
      });
      document.addEventListener("mousemove", mousemove);
    };
    // Add drag handle
    let dragHandle = mover.appendChild(document.createElement("drag"));
    let dragHandleIcon = document.createElement("img");
    dragHandle.className = classes.dragHandle;
    dragHandleIcon.src = DragHandleIcon;
    dragHandle.appendChild(dragHandleIcon);
    let popUpTitle = mover.appendChild(document.createElement("p"));
    popUpTitle.innerHTML = "Question";
    // Add close Icon
    let closer = mover.appendChild(document.createElement("closer"));
    let closeImage = document.createElement("img");
    closer.className = classes.closer;
    closeImage.src = CloseXIcon;
    closer.appendChild(closeImage);
    closer.onclick = (e) => {
      qNode.editor.destroy();
      qNode.editor = null;
      pop.parentNode.removeChild(pop);
    };

    let editorHolder = pop.appendChild(document.createElement("div"));
    editorHolder.className = classes.questionEditorHolder;

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
  }
};

export const questionReferenceMarkView =
  (questionMap: Dictionary<QuestionMapItem>) => (mark: Mark, view: EditorView) => {
    const mview = document.createElement("questionRef");
    mview.className = classes.questionRef;
    const qId = mark.attrs.questionId;
    mview.setAttribute("questionId", qId);
    let pop = null;
    // mview.onclick = (e) => {
    //   e.preventDefault();
    //   questionPopup(e.pageX, e.pageY, qId, questionMap, view);
    // };
    return { dom: mview };
  };

export const referenceToMarkView = (mark: Mark, view: EditorView) => {
  const mview = document.createElement("span");
  mview.className = classes.referenceTo;
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
  (qId: string, questionMap: Dictionary<QuestionMapItem>) => (view: EditorView) => {
    const elem = document.createElement("div");
    elem.className = classes.questionMark;
    elem.innerHTML = "?";
    elem.onmousedown = (e) => {
      e.preventDefault();
      e.stopPropagation();
      questionPopup(e.pageX, e.pageY, qId, questionMap, view);
    };
    return elem;
  };

let questionMarkPlugin = (questionMap: Dictionary<QuestionMapItem>) =>
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
            Decoration.widget(loc, questionMarkWidget(qId, questionMap), {
              stopEvent: (e: Event) => {
                return e.type === "click";
              },
            })
          );
        });
        return DecorationSet.create(state.doc, decorations);
      },
    },
  });


let addVerse = (bibRef: string, verses: string, position: number, state: EditorState, dispatch?: (tr: Transaction) => void) => {
  if (dispatch) {
    // Make the mark
    const text = textSchema.text(verses);
    const chunk = textSchema.nodes.chunk.create(null, text);
    const bibleText = textSchema.nodes.bibleText.create(null, chunk);

    const tr = state.tr.insert(position, bibleText);
    dispatch(tr);
    return true;
  }
};


let addVerseWidget = (position: number) => (view : EditorView) => {
  const elem = document.createElement("button");
  elem.innerHTML = "add verse";
  elem.onmousedown = (e) => {
    console.log(position);
    e.preventDefault();
    e.stopPropagation();
    const verses = prompt("verses: ");
    const text = prompt("text: ");
    if(verses && text) {
      addVerse(verses, text, position, view.state, view.dispatch);
    } else {
      alert("Missing input");
    }
  };
  return elem;
};

let addVersePlugin = () =>
  new Plugin({
    props: {
      decorations(state: EditorState) {
        const stuff = [];
        state.doc.descendants((node, position) => {
          if (node.type.name === "section") return true;
          if (node.type.name !== "studyBlocks") return false;
          stuff.push({ node, position });
        });
        const decorations = [];
        stuff.forEach(({ node, position }) => {
          decorations.push(
            Decoration.widget(position, addVerseWidget(position), {
              stopEvent: (e: Event) => {
                return e.type === "click";
              },
            })
          );
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

const addQuestion = (state: EditorState, dispatch?: (tr: Transaction) => void) => {
  const from = state.selection.from;
  const to = state.selection.to;
  if (from === to) {
    return false;
  }
  if (dispatch) {
    const r = newQuestionNode();
    const qId = r[0];
    const qNode = r[1];

    // Make the mark
    const qMark = textSchema.marks.questionReference.create({ questionId: qId });
    let tr = state.tr.addMark(from, to, qMark);

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
    // const sel = TextSelection.create(tr1.doc, pos+3);
    // const tr2 = tr1.setSelection(sel);
    dispatch(tr1);
    return true;
  }
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

interface QuestionMapItem {
  node: Node;
  getPos: () => number;
  editor: EditorView | null;
}

type notUndefined = string | number | boolean | symbol | object;

interface Dictionary<T extends notUndefined = notUndefined> {
  [key: string]: T | undefined;
}

export class P215Editor {
  state: EditorState;
  view: EditorView;
  questionMap: Dictionary<QuestionMapItem>;
  updateHanlders: Array<(change: any) => void>;

  constructor(initialState) {
    let node = Node.fromJSON(textSchema, initialState);
    this.updateHanlders = [];
    this.questionMap = {};
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
          "Mod-u": addQuestion,
        }),
        keymap(baseKeymap),
        currentChunkPlug,
        questionMarkPlugin(this.questionMap),
        addVersePlugin(),
        // referencePlugin
      ],
    });
  }

  addEditor(editorRoot: HTMLElement) {
    let that = this;
    this.view = new EditorView(editorRoot, {
      state: that.state,
      nodeViews: {
        section(node) {
          return new SectionView(node, sectionCounter++);
        },
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
      },
      markViews: {
        referenceTo: referenceToMarkView,
        questionReference: questionReferenceMarkView(that.questionMap),
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
        this.updateHanlders.forEach((handler) => {
          handler(transaction.doc.toJSON());
        });
      },
    });
  }

  removeEditor() {
    this.view.destroy();
  }

  addQuestion() {
    addQuestion(this.view.state, this.view.dispatch);
  }

  decreaseLevel() {
    decreaseLevel(this.view.state, this.view.dispatch);
  }

  undo() {
    const { state, dispatch } = this.view;
    undo(state, dispatch);
  }

  redo() {
    const { state, dispatch } = this.view;
    redo(state, dispatch);
  }

  toggleBold() {
    const { state, dispatch } = this.view;
    const markType = state.schema.marks.strong;
    toggleMark(markType)(state, dispatch);
  }

  toggleItalic() {
    const { state, dispatch } = this.view;
    const markType = state.schema.marks.em;
    toggleMark(markType)(state, dispatch);
  }

  toggleUnderline() {
    const { state, dispatch } = this.view;
    const markType = state.schema.marks.underline;
    toggleMark(markType)(state, dispatch);
  }

  setTextColor(color: string) {
    const { state, dispatch } = this.view;
    const { from, to } = state.selection;
    const markType = state.schema.marks.textColor;
    const attrs = { color };
    let tr = state.tr.removeMark(from, to, markType);
    tr.addMark(from, to, markType.create(attrs));
    dispatch(tr);
  }

  setHighlightColor(color: string) {
    const { state, dispatch } = this.view;
    const { from, to } = state.selection;
    const markType = state.schema.marks.highlightColor;
    const attrs = { color };
    let tr = state.tr.removeMark(from, to, markType);
    tr.addMark(from, to, markType.create(attrs));
    dispatch(tr);
  }

  removeHighlightColor() {
    const { state, dispatch } = this.view;
    const { from, to } = state.selection;
    const markType = state.schema.marks.highlightColor;
    const tr = state.tr.removeMark(from, to, markType);
    dispatch(tr);
  }

  clearFormatting() {
    const { state, dispatch } = this.view;
    const { tr, schema, selection } = state;
    const { from, to } = selection;
    tr.removeMark(from, to);
    state.doc.nodesBetween(from, to, (node, pos) => {
      if (node.type === schema.nodes.section) {
        return false;
      }
      if (node.isTextblock && node.type !== schema.nodes.paragraph) {
        tr.setNodeMarkup(pos, schema.nodes.paragraph);
      }
      return true;
    });
    if (tr.docChanged) {
      dispatch(tr);
    }
  }

  increaseLevel() {
    increaseLevel(this.view.state, this.view.dispatch);
  }

  addSection() {
    addSection(this.view.state, this.view.dispatch);
  }

  insertLink(url: string, title: string = "") {
    const { state, dispatch } = this.view;
    const { tr, selection } = state;
    const link = state.schema.marks.link.create({ href: url, title });
    tr.addMark(selection.from, selection.to, link);
    dispatch(tr);
  }
  deleteSection(sectionIndex: number) {
    sectionCounter = sectionIndex + 1;
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

  onUpdate(f: (change: any) => void) {
    this.updateHanlders.push(f);
  }
}
