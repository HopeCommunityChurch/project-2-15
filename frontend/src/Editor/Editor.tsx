import {EditorState} from "prosemirror-state"
import {AttrStep} from "prosemirror-transform"
import {EditorView} from "prosemirror-view"
import {undo, redo, history} from "prosemirror-history"
import {keymap} from "prosemirror-keymap"
import {Schema, Node} from "prosemirror-model"
import {baseKeymap} from "prosemirror-commands"
import "./styles.css"
const textSchema = new Schema({
  nodes: {
    doc: {
      content: "chunk+"
    },
    chunk: {
      content: "text*",
      attrs: { level: { default: 0 } },
      toDOM: (node) => {
        return [
          "p",
          {
            "class": "chunk",
            "level": node.attrs.level,
            "style": "margin-left:" + node.attrs.level + "em;",
          },
          0
        ]
      },
    },
    text: {inline: true},
  }
});

const increaseLevel = (state : EditorState, dispatch ) => {
  let type = textSchema.nodes.chunk;
  let from = state.selection.from;
  let to = state.selection.to;
  let toTransform  = [];
  // Why is this stupid?
  state.doc.nodesBetween(from, to, (node, pos) => {
    if (node.type != type)
      return false;
    toTransform.push({pos, node});
  });
  if (dispatch)
    dispatch(
      toTransform.reduce((pre, {pos, node}) => {
        return pre.setNodeMarkup(pos, type, {level: node.attrs.level+1}, null);
      }, state.tr)
    );
  return true;
};

const descreaseLevel = (state : EditorState, dispatch) => {
  let type = textSchema.nodes.chunk;
  let from = state.selection.from;
  let to = state.selection.to;
  let toTransform  = [];
  // Why is this stupid?
  state.doc.nodesBetween(from, to, (node, pos) => {
    if (node.type != type)
      return false;
    toTransform.push({pos, node});
  });
  let nextState = toTransform.reduce((pre, {pos, node}) => {
    let level = node.attrs.level;
    let nextLevel = (level !=0)? level-1 : level;
    return pre.setNodeMarkup(pos, type, {level: nextLevel}, null);
  }, state.tr)
  if (dispatch) dispatch( nextState);
  return true;
};



let defualtTxt = `
{"type":"doc","content":[{"type":"chunk","attrs":{"level":0},"content":[{"type":"text","text":"21 But now God has shown us a way to be made right with him without keeping the requirements of the law, as was promised in the writings of Moses and the prophets long ago. 22 We are made right with God by placing our faith in Jesus Christ. And this is true for everyone who believes, no matter who we are. 23 For everyone has sinned; we all fall short of God’s glorious standard.24 Yet God, in his grace, freely makes us right in his sight. He did this through Christ Jesus when he freed us from the penalty for our sins. 25 For God presented Jesus as the sacrifice for sin. People are made right with God when they believe that Jesus sacrificed his life, shedding his blood. This sacrifice shows that God was being fair when he held back and did not punish those who sinned in times past, 26 for he was looking ahead and including them in what he would do in this present time. God did this to demonstrate his righteousness, for he himself is fair and just, and he makes sinners right in his sight when they believe in Jesus. 27 Can we boast, then, that we have done anything to be accepted by God? No, because our acquittal is not based on obeying the law. It is based on faith. 28 So we are made right with God through faith and not by obeying the law. 29 After all, is God the God of the Jews only? Isn’t he also the God of the Gentiles? Of course he is. 30 There is only one God, and he makes people right with himself only by faith, whether they are Jews or Gentiles. 31 Well then, if we emphasize faith, does this mean that we can forget about the law? Of course not! In fact, only when we have faith do we truly fulfill the law."}]}]}
`;

let node = Node.fromJSON(textSchema, JSON.parse(defualtTxt));

let state = EditorState.create({
  schema: textSchema,
  doc: node,
  plugins: [
    history(),
    keymap({
      "Mod-z": undo,
      "Mod-y": redo,
      "Tab": increaseLevel,
      "Shift-Tab": descreaseLevel,
    }),
    keymap(baseKeymap),
  ],
});

let view = new EditorView(document.getElementById('editorRoot'), {
  state,
  dispatchTransaction: (transaction) => {
    // console.log(JSON.stringify(transaction.doc.toJSON()));
    let newState = view.state.apply(transaction);
    view.updateState(newState);
  }
});

