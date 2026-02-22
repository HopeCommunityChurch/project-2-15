import {
  getCurrentTextAndHighlightColors,
  increaseLevel,
  toggleBold,
  toggleUnderline,
  toggleItalic,
  decreaseLevel,
  addGeneralStudyBlock,
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
import { Step } from "prosemirror-transform";
import { baseKeymap } from "prosemirror-commands";
// import AddScriptureIcon from "../Assets/add-scripture.svg";
import {
  questionHighlightPlugin,
} from "./QuestionHighlightPlugin";
import { otherCursorPlugin, setSelection } from "./OtherCursorPlugin";
import {
  QuestionMapItem,
  Dictionary,
  QuestionsView,
  QuestionView,
  QuestionAnswerView,
  questionReferenceMarkView,
  referenceToMarkView,
  questionMarkPlugin,
  addQuestion,
  deleteQuestionSelection,
  deleteAnswerSelection,
  removeQuestion,
} from "./QuestionPlugin";

// import CloseXIcon from "../Assets/x.svg";
import { v4 as uuidv4 } from "uuid";

// var blockMap : Dictionary<BlockMapItem> = {};


export class EditorStudyBlocksEdit extends Event {
  studyBlocks : Array<{node : Node, pos : number }>;
  studyBlockPos : number;
  constructor(studyBlocks : Array<{node : Node, pos : number }>, pos:number) {
    super("studyblocks-edit");
    this.studyBlocks = studyBlocks;
    this.studyBlockPos = pos;
  }
}


class StudyBlocksView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  constructor(
    editor : P215Editor,
    node: Node,
    view: EditorView,
    getPos: () => number,
  ) {
    // Create a container div
    this.dom = document.createElement("div");
    this.dom.className = "studyBlocksContainer";

    // Create the table
    const table = document.createElement("table");
    table.className = "studyBlocks";

    // Create and configure the Pencil icon
    const editIcon = new Image();
    editIcon.src = "/static/img/gray-pencil-in-circle.svg";
    editIcon.className = "studyBlockEditPencil";

    editIcon.addEventListener("click", () => {
      let sectionNode : Node = null;
      let sectionPos = null;
      view.state.doc.nodesBetween(getPos(), getPos(), (node, pos) => {
        if (node.type.name === "section") {
          sectionNode = node;
          sectionPos = pos;
          return true;
        }
      });
      if (sectionNode === null) {
        console.error("unable to get section for editing study block");
        return;
      }
      let studyBlocks = [];
      sectionNode.descendants((node, pos, parent) => {
        if(parent.type.name === "section" && node.type.name === "studyBlocks") {
          return true;
        } else if(parent.type.name === "section") {
          return false
        }
        if(parent.type.name === "studyBlocks") {
          studyBlocks.push({node, pos});
          return false;
        }
        return false;
      });
      let event = new EditorStudyBlocksEdit(studyBlocks, getPos());
      editor.dispatchEvent(event);
    });

    this.dom.appendChild(editIcon);

    // Set the table as the main content
    this.contentDOM = table;
    this.dom.appendChild(table);
  }
}

class BibleTextView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;

  constructor(
    node: Node,
  ) {
    this.dom = document.createElement("div");
    this.dom.className = "bibleText";

    const header = document.createElement("h3");
    header.innerText = node.attrs.verses;
    this.dom.appendChild(header);

    const body = document.createElement("div");
    this.contentDOM = body;
    this.dom.appendChild(body);
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
    this.contentDOM.className = "studyBlockHeaderDiv";
    this.dom.appendChild(this.contentDOM);
  }
  update(node: Node) {
    this.node = node;
    return true;
  }
}

export class ChunkView implements NodeView {
  dom: HTMLElement;
  contentDOM: HTMLElement;
  button: HTMLElement;

  constructor(node: Node, view: EditorView, getPos: () => number) {
    this.dom = document.createElement("p");
    this.dom.className = "outerChunk";
    this.contentDOM = document.createElement("p");
    this.contentDOM.className = "chunk";
    const level = node.attrs.level;
    if (level != 0) {
      const color = "--indent" + (level % 5 + 1);
      this.contentDOM.style.borderLeft = `3px solid var(${color})`;
      this.contentDOM.style.paddingLeft = "6px";
      this.contentDOM.style.marginLeft = `${level}em`;
    }
    this.contentDOM.setAttribute("level", node.attrs.level);
    this.dom.appendChild(this.contentDOM);
  }

}



let hideOnlyOneBibleTextPlugin = new Plugin({
  props: {
    decorations(state: EditorState) {
      const decorations = [];
      const questions = {};
      state.doc.forEach( (section, offset) => {
        let count = 0;
        section.forEach( (node) => {
          if (node.type.name === "bibleText") count++;
        });
        if (count > 1) {
          decorations.push(
            Decoration.node(offset, offset + section.nodeSize, {
              class: "showBibleTextHeader",
            })
          );
        } else {
          decorations.push(
            Decoration.node(offset, offset + section.nodeSize, {
              class: "hideBibleTextHeader",
            })
          );
        }
      });

      return DecorationSet.create(state.doc, decorations);
    },
  },
});




const verseRefWidget = (verse) => (view: EditorView, getPos: () => number | undefined) => {
  const elem = document.createElement("span");
  elem.onmousedown = (e) => {
    e.preventDefault();
    e.stopPropagation();
    const pos = getPos();
    if (pos != null) {
      const tr = view.state.tr.setSelection(
        TextSelection.create(view.state.doc, pos)
      );
      view.dispatch(tr);
      view.focus();
    }
  };
  elem.ondblclick = (e) => {
    e.preventDefault();
    e.stopPropagation();
    let book = verse.book.replace(" ", "_").toLowerCase();
    let url = "https://biblehub.com/" + book + "/" + verse.chapter + "-" + verse.verse + ".htm";
    window.open(url, "_blank").focus();
    view.focus();
  };
  elem.className = "verseRef";
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
        Decoration.widget(position, verseRefWidget(verse.attrs), {
          key: currentVerseKey,
          ignoreSelection: true,
          side: -1,
          stopEvent: (e: Event) => e.type === "mousedown" || e.type === "click",
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
  placeholderElement.className = "bibleTextPlaceholder";
  placeholderElement.innerHTML = `
    <em>
      Place your cursor in this section and click the "Add Scripture" button
    </em>
    <img src="${window.base}/static/img/add-scripture.svg" alt="Add Scripture Icon">
    <em>above to add your verses here</em>
  `;

  placeholderElement.onclick = () => {
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
      let headerLength = 0;
      node.forEach((childNode) => {
        if (childNode.type.name === "sectionHeader") {
          headerLength = childNode.content.size;
          return;
        }
      });

      const regex = /section\(sectionHeader\("([^"]+)"\)/g;
      let match;
      while ((match = regex.exec(node)) !== null) {
        const contentBetweenQuotes = match[1];

        const hasBibleText = node.content.content.some((child) => child.type.name === "bibleText");

        if (!hasBibleText) {
          const placeholderDecoration = Decoration.widget(
            pos + contentBetweenQuotes.length + 3,
            mkPlaceholderElement(pos + headerLength + 2)
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

type RemoteThingy = {
  send(steps: any);
};

type SectionDiff = {
  head: number;
  anchor: number;
};

type TransactionDiff = {
  steps: any[];
  selection: SectionDiff;
};

function cleanupExtraStudyBlocks (initDoc : any) : any {
  const newArray = initDoc.content.map( (section) => {
    return {
      type: "section",
      content: section.content.filter((node) =>
        !(node.type == "studyBlocks" && !node.hasOwnProperty("content"))
      )
    }
  });
  const result = {
    type: "doc",
    content: newArray,
  }
  return result;
}

type DispatchFunc = (tr : Transaction) => void;

export class P215Editor extends EventTarget {
  state: EditorState;
  editable: boolean;
  view: EditorView;
  questionMap: Dictionary<QuestionMapItem>;
  updateHanlders: Array<(change: any) => void>;
  updateStateHanlders: Array<(change: EditorState) => void>;
  remoteThings: RemoteThingy;

  currentEditor : EditorView;

  constructor({
    initDoc,
    editable,
    remoteThings,
  }) {
    super();
    this.editable = editable;
    this.remoteThings = remoteThings;
    let node = Node.fromJSON(textSchema, cleanupExtraStudyBlocks(initDoc));
    this.updateHanlders = [];
    this.updateStateHanlders = [];
    this.questionMap = {};


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
          "Mod-s": () => {return true;}, // Have ctrl-s do nothing
          "Mod-b": toggleMark(textSchema.marks.strong),
          "Mod-i": toggleMark(textSchema.marks.em),
          "Mod-u": toggleMark(textSchema.marks.underline),
        }),
        keymap(baseKeymap),
        questionMarkPlugin(this.questionMap, (view) => this.currentEditor = view),
        sectionIdPlugin,
        verseReferencePlugin,
        preventUpdatingMultipleComplexNodesSelectionPlugin,
        bibleTextPlaceholderPlugin,
        questionHighlightPlugin,
        otherCursorPlugin,
        hideOnlyOneBibleTextPlugin,
        keymap({
          Tab: (state: EditorState, dispatch?: (tr: Transaction) => void) => {
            return true;
          }
        })
        // referencePlugin,
      ],
    });
  }

  addQuestionCommand = (state, dispatch) => {
    return addQuestion(
            state,
            dispatch,
            this.questionMap,
            this.view,
            (view) => this.currentEditor = view
          );
  };

  addEditor(editorRoot: HTMLElement) {
    let that = this;
    this.view = new EditorView(editorRoot, {
      state: that.state,
      editable: () => that.editable,
      handlePaste: this.handlePaste.bind(this),
      handleDOMEvents: {
        focus: () => {
          this.currentEditor = this.view;
        },
      },
      nodeViews: {
        bibleText(node) {
          return new BibleTextView(node);
        },
        studyBlocks(node, view, getPos) {
          return new StudyBlocksView(
            that,
            node,
            view,
            getPos,
          );
        },
        questions(node, view, getPos) {
          return new QuestionsView(node, view, getPos);
        },
        chunk(node, view, getPos) {
          return new ChunkView(node, view, getPos);
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
        that.view.updateState(newState);

        let steps = null;
        if (transaction.docChanged) {
          this.updateHanlders.forEach((handler) => {
            try {
              handler(newState.doc);
            } catch(e) {
              console.error(e);
            }
          });
          steps = transaction.steps.map((st) => st.toJSON());
        }


        this.updateStateHanlders.forEach((handler) => {
          try {
            handler(newState);
          } catch(e) {
            console.error(e);
          }
        });

        const head = newState.selection.head;
        const anchor = newState.selection.anchor;
        if (this.remoteThings !== null && this.remoteThings.send) {
          this.remoteThings.send({
            steps: steps,
            selection: {
              anchor: anchor,
              head: head,
            },
          });
        }
      },
    });
    this.currentEditor = this.view;
  }

  dispatchSteps(changeDiff: TransactionDiff) {
    if (changeDiff.steps != null) {
      const steps = changeDiff.steps.map((st) => Step.fromJSON(textSchema, st));
      const tr = this.view.state.tr;
      steps.forEach((st) => tr.step(st));
      this.view.dispatch(tr);
    }
    setSelection(changeDiff.selection, this.view.state, this.view.dispatch);
  }

  applyDispatch(func : (state: EditorState, dispatch : DispatchFunc) => void) {
    const state = this.currentEditor.state;
    const dispatch = this.currentEditor.dispatch;
    func(state, dispatch);
    this.currentEditor.focus();
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
        (view) => this.currentEditor = view
      );
      this.view.focus();
    }
  }

  getCurrentTextAndHighlightColors(setHighlightFillColor, setTextFillColor) {
    getCurrentTextAndHighlightColors(this.view.state, setHighlightFillColor, setTextFillColor);
  }

  getCurrentVerse() {
    const { state } = this.view;
    const { selection } = state;
    let headerText = "";

    // Traverse up the document structure from the selection to find the section
    state.doc.nodesBetween(selection.from, selection.to, (node) => {
      if (node.type.name === "section") {
        // Find the sectionHeader child of this section
        node.forEach((childNode) => {
          if (childNode.type.name === "sectionHeader") {
            headerText = childNode.textContent;
          }
        });
        return false; // Stop traversing further
      }
    });

    return headerText;
  }

  onStateUpdate(f: (change: EditorState) => void) {
    if (this.editable) {
      this.updateStateHanlders.push(f);
    }
  }
  onUpdate(f: (change: any) => void) {
    if (this.editable) {
      this.updateHanlders.push(f);
    }
  }

  scrollTo (index : number) : void {
    const state = this.view.state;
    let position = null;
    state.doc.descendants( (_1, pos, _2, i) => {
      if (i === index) {
        position = pos;
      }
      return false;
    });

    if (position == null) {
      return;
    }

    const selection = new TextSelection(this.view.state.doc.resolve(position+2));
    const tr = state.tr.setSelection(selection)
    this.view.dispatch(tr);

    const elem = document.getElementById(`section-${index}`);
    if (elem) {
      elem.scrollIntoView({behavior: "smooth"});
    }
    this.view.focus();
  }
}


export class EditorAttached extends Event {
  editor : P215Editor
  constructor(editor : P215Editor) {
    super("editorAttached");
    this.editor = editor;
  }
}

