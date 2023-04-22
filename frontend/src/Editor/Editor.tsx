import {EditorState, Plugin, PluginKey, Transaction, EditorStateConfig, Selection} from "prosemirror-state"
import {EditorView, NodeView, DecorationSet, Decoration} from "prosemirror-view"
import {undoItem, redoItem, MenuItem, MenuItemSpec, menuBar} from "prosemirror-menu"
import {undo, redo, history} from "prosemirror-history"
import {keymap} from "prosemirror-keymap"
import {Schema, Node, Mark, Fragment} from "prosemirror-model"
import {StepMap} from "prosemirror-transform"
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
      attrs : { referenceId : {default: null}, color: { default : "red"}},
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
  outerView : EditorView
  innerView : EditorView
  getPos : () => number


  constructor(node: Node, view: EditorView, getPos : () => number) {
    this.getPos = getPos;
    this.outerView = view;
    this.node = node;
    this.dom = document.createElement("chunkComment");
    this.dom.className = classes.chunkButton;
    this.popup = null;
    this.dom.onclick = (e) => {
      if(!this.popup) this.open();
    }
  }

  close () {
    if (this.popup) {
      this.innerView.destroy();
      this.innerView = null;
      this.dom.textContent = "";
      // I don't know why I have to do this. Probably because of some magic in
      // prosemirror.
      setTimeout( () => {
        this.popup = null;
      }, 0);
    }
  }

  update(node : Node) {
    // if (!node.sameMarkup(this.node)) return false
    this.node = node
    if (this.innerView) {
      let state = this.innerView.state
      let start = node.content.findDiffStart(state.doc.content)
      if (start != null) {
        let {a: endA, b: endB} = node.content.findDiffEnd(state.doc.content)
        let overlap = start - Math.min(endA, endB)
        if (overlap > 0) { endA += overlap; endB += overlap }
        this.innerView.dispatch(
          state.tr
            .replace(start, endB, node.slice(start, endA))
            .setMeta("fromOutside", true))
      }
    }
    return true
  }

  open () {
    let pop = this.dom.appendChild(document.createElement("div"));
    this.popup = pop;
    pop.className = classes.chunkCommentPopup;

    let closeBut = pop.appendChild(document.createElement("button"));
    closeBut.innerText = "Close"
    closeBut.onclick = (e) => {
      e.preventDefault();
      this.close();
    };

    let addRefBut = pop.appendChild(document.createElement("button"));
    addRefBut.innerText = "Add Reference"
    addRefBut.onclick = (e) => {
      console.log("test");
      e.preventDefault();
      const rId = crypto.randomUUID();
      const meta = {
        addedReference: true,
        currentSelection:null,
        referenceId: rId,
      };
      const tr =
        this.outerView.state.tr
          .setNodeAttribute(this.getPos(), "referenceId", rId)
          .setMeta(referencePluginKey, meta);
      this.outerView.dispatch(tr);
    }

    let editorHolder = pop.appendChild(document.createElement("div"));
    this.innerView = new EditorView( editorHolder, {
      state: EditorState.create({
        doc: this.node,
      }),
      dispatchTransaction: this.dispatchInner.bind(this),
    });
  }

  dispatchInner(tr) {
    let {state, transactions} = this.innerView.state.applyTransaction(tr)
    this.innerView.updateState(state)

    if (!tr.getMeta("fromOutside")) {
      let outerTr = this.outerView.state.tr, offsetMap = StepMap.offset(this.getPos() + 1)
      for (let i = 0; i < transactions.length; i++) {
        let steps = transactions[i].steps
        for (let j = 0; j < steps.length; j++)
          outerTr.step(steps[j].map(offsetMap))
      }
      if (outerTr.docChanged) this.outerView.dispatch(outerTr)
    }
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
      if ( meta && meta.addedReference === true) {
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

      if (!selection.empty)
        return null;

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
});


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


let view = new EditorView(document.getElementById('editorRoot'), {
  state,
  nodeViews: {
    section(node) {
      return new SectionView(node);
    },
    questions(node) {
      return new QuestionsView(node);
    },
    chunk(node, view, getPos) {
      return new ChunkView(node, view, getPos);
    },
    chunkComment(node, view, getPos) {
      return new ChunkCommentView(node, view, getPos);
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

