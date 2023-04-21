import {EditorState, Plugin, PluginKey, Transaction, EditorStateConfig, Selection} from "prosemirror-state"
import {EditorView, NodeView, DecorationSet, Decoration} from "prosemirror-view"
import {undoItem, redoItem, MenuItem, MenuItemSpec, menuBar} from "prosemirror-menu"
import {undo, redo, history} from "prosemirror-history"
import {keymap} from "prosemirror-keymap"
import {Schema, Node, Mark, Fragment} from "prosemirror-model"
import {baseKeymap} from "prosemirror-commands"
import "./styles.css"
import * as classes from "./styles.module.css"
import defaultText from "./default.json"



export const textSchema = new Schema({
  nodes: {
    doc: {
      content: "section*"
    },
    section: {
      content: "studyElement*",
      isolating: true,
      defining: true,
      attrs: { book: { default: "Genesis" }, verses: { default: "1:1" }},
    },
    bibleText: {
      content: "chunk*",
      group: "studyElement",
      isolating: true,
      defining: true,
      toDOM: () => {
        return [
          "div",
          {
            "class": classes.bibleText,
          },
          0
        ]
      }
    },
    questions: {
      content: "question+",
      group: "studyElement",
      isolating: true,
      defining: true,
    },
    question: {
      content: "text*",
      toDOM: () => {
        return [
          "li",
          { "class": classes.question},
          0
        ]
      }
    },
    chunkComment : {
      content: "text*",
      inline: true,
      atom: true,
      draggable: false,
      defining: true,
      selectable: false,
      attrs : { reference : {default: null}, color: { default : "red"}},
      toDOM : () => ["chunkComment", 0],
      parseDOM: [{tag: "chunkComment"}]
    },
    chunk: {
      content: "(chunkComment | text)*",
      attrs: { level: { default: 0 } },
      toDOM: (node) => {
        return [
          "div",
          {
            "class": classes.chunk,
            "level": node.attrs.level,
            "style": "margin-left:" + 1.5 * node.attrs.level + "em;",
          },
          0
        ]
      },
    },
    // chunkText: {
    //   content: "text*",
    //   inline: true,
    //   toDom: () => {
    //     return ["span", 0]
    //   },
    // },
    text: {inline: true},
  },
  marks: {
    reference: {
      attrs: { referenceId: {} },
      excludes: "",
      toDOM: (mark) => {
        return [
          "span",
          {
            "data-type": "reference",
            "class": classes.reference,
            "referenceId": mark.attrs.referenceId,
          },
          0
        ]
      }
    },
    referenceTo: {
      attrs: { referenceId: {} },
      excludes: "",
    },
  },
});

export class SectionView implements NodeView {
  dom : HTMLElement
  contentDOM : HTMLElement
  constructor (node : Node) {
    this.dom = document.createElement("div");
    this.dom.className = classes.section;
    const header = document.createElement("h2");
    header.setAttribute("contenteditable", "false");
    header.innerText = node.attrs.book + " " + node.attrs.verses;
    this.dom.appendChild(header)
    this.contentDOM = document.createElement("div");
    this.contentDOM.className = classes.content;
    this.dom.appendChild(this.contentDOM);
  }
}

export class QuestionsView implements NodeView {
  dom : HTMLElement
  contentDOM : HTMLElement
  constructor (node : Node) {
    this.dom = document.createElement("div");
    this.dom.className = classes.questions;
    const header = document.createElement("h3");
    header.setAttribute("contenteditable", "false");
    header.innerText = "Questions"
    this.dom.appendChild(header)
    this.contentDOM = document.createElement("ul");
    this.contentDOM.className = classes.content;
    this.dom.appendChild(this.contentDOM);
  }
}

export class ChunkView implements NodeView {
  dom : HTMLElement
  contentDOM : HTMLElement
  button : HTMLElement

  constructor(node: Node, view: EditorView, getPos: () => number) {
    this.dom = document.createElement("p");

    this.setPadding(node);


    this.dom.className = classes.outerChunk;
    this.contentDOM = document.createElement("p");
    this.contentDOM.className = classes.chunk;
    this.contentDOM.setAttribute("level", node.attrs.level);
    this.dom.appendChild(this.contentDOM);

    this.dom.onmouseenter = (e) => {
      let hasChunkComment = this.hasComment(node);

      if (hasChunkComment) return;

      e.preventDefault();
      let widget = Decoration.widget(getPos()+1, () => {
        const but = document.createElement("div");
        but.className = classes.chunkButton;
        but.onclick = () => {
          const chunkCom = textSchema.nodes.chunkComment.create({
            content: Fragment.empty,
          });
          view.dispatch(view.state.tr.insert(getPos()+1, chunkCom));
        };
        return but;
      });
      view.setProps({
        decorations: (state) => {
          return DecorationSet.create(state.doc, [
            widget
          ]);
        },
      });
    }
  }

  hasComment (node : Node) {
    let hasChunkComment = false;
    node.descendants((node:Node) => {
      if(node.type.name == "chunkComment") hasChunkComment = true;
      return false;
    });
    return hasChunkComment;
  }

  setPadding (node : Node) {
    let style = "padding-left: calc(" + 1.5 * node.attrs.level + "em + 7px);";
    this.dom.setAttribute("style", style);
  }
}

export class ChunkCommentView implements NodeView {
  dom : HTMLElement
  contentDOM : HTMLElement
  popup : HTMLElement
  node : Node
  view : EditorView


  constructor(node: Node, view: EditorView) {
    this.dom = document.createElement("chunkComment");
    this.dom.className = classes.chunkButton;
    this.popup = null;
    this.dom.onclick = (e) => {
      if(!this.popup) this.open();
    }
  }

  open () {
    let pop = this.dom.appendChild(document.createElement("div"));
    pop.className = classes.chunkCommentPopup;
    this.popup = pop;
  }
}

export const referenceToMarkView = (mark : Mark, view : EditorView) => {
  const mview = document.createElement("span");
  mview.className = classes.referenceTo;
  const rId = mark.attrs.referenceId
  mview.setAttribute("referenceId", rId);
  let qselector = 'span[data-type="reference"][referenceId="'+ rId +'"]'
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
  return { dom: mview }
}
