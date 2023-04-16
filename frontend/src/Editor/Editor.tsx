import {EditorState, Plugin, PluginKey, Transaction, EditorStateConfig, Selection} from "prosemirror-state"
import {EditorView, NodeView, DecorationSet, Decoration} from "prosemirror-view"
import {undoItem, redoItem, MenuItem, MenuItemSpec, menuBar} from "prosemirror-menu"
import {undo, redo, history} from "prosemirror-history"
import {keymap} from "prosemirror-keymap"
import {Schema, Node, Mark} from "prosemirror-model"
import {baseKeymap} from "prosemirror-commands"
import "./styles.css"
import * as classes from "./styles.module.css"
import defaultText from "./default.json"


const textSchema = new Schema({
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
    chunk: {
      content: "text*",
      attrs: { level: { default: 0 } },
      toDOM: (node) => {
        return [
          "p",
          {
            "class": classes.chunk,
            "level": node.attrs.level,
            "style": "margin-left:" + 1.5 * node.attrs.level + "em;",
          },
          0
        ]
      },
    },
    text: {inline: true},
  },
  marks: {
    reference: {
      attrs: { referenceId: {} },
      toDOM: (mark) => {
        return [
          "span",
          {
            "data-type": "reference",
            "referenceId": mark.attrs.referenceId,
          },
          0
        ]
      }
    },
    referenceTo: {
      attrs: { referenceId: {} },
    },
  },
});

const nodeIsChunk = (node : Node) => {
  if (node.type.name === "section")
    return true;
  if (node.type.name === "bibleText")
    return true;
  if (node.type.name != "chunk")
    return false;
}

const increaseLevel = (state : EditorState, dispatch?: ((tr: Transaction) => void) ) => {
  let from = state.selection.from;
  let to = state.selection.to;
  let toTransform  = [];
  // Why is this stupid?
  state.doc.nodesBetween(from, to, (node, pos) => {
    const result = nodeIsChunk(node)
    if (result === true)
      return true;
    if (result === false)
      return false;
    toTransform.push({pos, node});
    return false;
  });
  if (toTransform.length == 0) return false;
  let type = textSchema.nodes.chunk;
  if (dispatch)
    dispatch(
      toTransform.reduce((pre, {pos, node}) => {
        return pre.setNodeMarkup(pos, type, {level: node.attrs.level+1}, null);
      }, state.tr)
    );
  return true;
};

const decreaseLevel = (state : EditorState, dispatch?: ((tr: Transaction) => void) ) => {
  let type = textSchema.nodes.chunk;
  let from = state.selection.from;
  let to = state.selection.to;
  let toTransform  = [];
  // Why is this stupid?
  state.doc.nodesBetween(from, to, (node, pos) => {
    const result = nodeIsChunk(node)
    if (result === true)
      return true;
    if (result === false)
      return false;
    toTransform.push({pos, node});
    return false;
  });
  if (toTransform.length == 0) return false;
  let nextState = toTransform.reduce((pre, {pos, node}) => {
    let level = node.attrs.level;
    let nextLevel = (level !=0)? level-1 : level;
    return pre.setNodeMarkup(pos, type, {level: nextLevel}, null);
  }, state.tr)
  if (dispatch) dispatch( nextState);
  return true;
};


const addReference = (state : EditorState, dispatch?: ((tr: Transaction) => void), view?: EditorView ) => {
  let currentSelection = state.selection;
  let from = state.selection.from;
  let to = state.selection.to;
  if (dispatch) {
    const rId = crypto.randomUUID();
    let referenceToMarkType = textSchema.marks.referenceTo;
    const mark = referenceToMarkType.create({referenceId: rId})
    let newState = state.tr.addMark(from, to, mark).setMeta(referencePluginKey, {addedReference: true, currentSelection, referenceId: rId});
    dispatch(newState);
  }
  return true;
};

interface ReferencePluginState {
  isLooking : boolean;
  selection?: Selection;
  referenceId? : string;
}

let referencePluginKey = new PluginKey<ReferencePluginState>("referencePlugin");

let referencePlugin = new Plugin({
  key: referencePluginKey,
  state: {
    init () {
      return {isLooking : false}
    },
    apply ( tr : Transaction, value : ReferencePluginState) {
      const meta = tr.getMeta(referencePluginKey);
      if ( meta && meta.addedReference === true && meta.currentSelection) {
        console.log(meta);
        return {
          isLooking : true,
          selection : meta.currentSelection,
          referenceId: meta.referenceId,
        };
      } else if (meta && meta.referenceDone) {
        return {isLooking : false};
      } else {
        return {
          isLooking : value.isLooking,
          selection : value.selection,
          referenceId : value.referenceId,
        };
      }
    }
  },
  props: {
    handleDOMEvents: {
      "mouseup": (view : EditorView, e) => {
        const state = referencePluginKey.getState(view.state)
        if (state.isLooking) {
          const selection = view.state.selection;
          const from = selection.from;
          const to = selection.to;
          if (selection.empty) {
            return false;
          }
          e.preventDefault();
          let referenceMarkType = textSchema.marks.reference;
          const mark = referenceMarkType.create({referenceId: state.referenceId});
          const newstate = view.state.tr
                            .addMark(from, to, mark)
                            .setMeta(referencePluginKey, {referenceDone: true});
          view.dispatch(newstate);
          return true;
        } else {
          return false;
        }
      },
    },
  },
});

let node = Node.fromJSON(textSchema, defaultText);

let currentChunkPlug = new Plugin({
  props: {
    decorations(state : EditorState) {
      const selection = state.selection;
      const decorations = [];

      state.doc.nodesBetween(selection.from, selection.to, (node, position) => {
        const result = nodeIsChunk(node)
        if (result === true)
          return true;
        if (result === false)
          return false;
        decorations.push(Decoration.node(position, position + node.nodeSize, {class: classes.selected}));
      });

      return DecorationSet.create(state.doc, decorations);
    }
  }
})

const indentMenuItem : MenuItemSpec = {
  run: increaseLevel,
  select: increaseLevel,
  label: "indent",
};

const backIndentMenuItem : MenuItemSpec = {
  run: decreaseLevel,
  select: decreaseLevel,
  label: "unindent",
};


const addReferenceMenuItem : MenuItemSpec = {
  run: addReference,
  select: addReference,
  label: "add reference",
};

const menuStuff = [
  undoItem,
  redoItem,
  // new MenuItem(indentMenuItem),
  // new MenuItem(backIndentMenuItem),
  new MenuItem(addReferenceMenuItem),
];

let state = EditorState.create({
  schema: textSchema,
  doc: node,
  plugins: [
    history(),
    keymap({
      "Mod-z": undo,
      "Mod-y": redo,
      "Tab": increaseLevel,
      "Shift-Tab": decreaseLevel,
    }),
    keymap(baseKeymap),
    currentChunkPlug,
    menuBar({content: [menuStuff]}),
    referencePlugin
  ],
});

class SectionView implements NodeView {
  dom : HTMLElement
  contentDOM : HTMLElement
  constructor (node : Node) {
    this.dom = document.createElement("div");
    this.dom.className = classes.section;
    const header = document.createElement("h2");
    header.innerText = node.attrs.book + " " + node.attrs.verses;
    this.dom.appendChild(header)
    this.contentDOM = document.createElement("div");
    this.contentDOM.className = classes.content;
    this.dom.appendChild(this.contentDOM);
  }
}

class QuestionsView implements NodeView {
  dom : HTMLElement
  contentDOM : HTMLElement
  constructor (node : Node) {
    this.dom = document.createElement("div");
    this.dom.className = classes.questions;
    const header = document.createElement("h3");
    header.innerText = "Questions"
    this.dom.appendChild(header)
    this.contentDOM = document.createElement("ul");
    this.contentDOM.className = classes.content;
    this.dom.appendChild(this.contentDOM);
  }
}

const referenceToMarkView = (mark : Mark) => {
  const mview = document.createElement("span");
  mview.className = classes.referenceTo;
  const rId = mark.attrs.referenceId
  mview.setAttribute("referenceId", rId);
  let qselector = 'span[data-type="reference"][referenceId="'+ rId +'"]'
  mview.onmouseenter = (e) => {
    e.preventDefault();
    var references = document.querySelectorAll(qselector);
    references.forEach( (r) => {
      r.classList.add(classes.referenceTo);
    })
  };
  mview.onmouseleave = (e) => {
    e.preventDefault();
    var references = document.querySelectorAll(qselector);
    references.forEach( (r) => {
      r.classList.remove(classes.referenceTo);
    })
  };
  return { dom: mview }
}


let view = new EditorView(document.getElementById('editorRoot'), {
  state,
  nodeViews: {
    section(node) {
      return new SectionView(node);
    },
    questions(node) {
      return new QuestionsView(node);
    },
  },
  markViews: {
    referenceTo: referenceToMarkView,
  },
  dispatchTransaction: (transaction) => {
    // console.log(JSON.stringify(transaction.doc.toJSON()));
    let newState = view.state.apply(transaction);
    view.updateState(newState);
  }
});

