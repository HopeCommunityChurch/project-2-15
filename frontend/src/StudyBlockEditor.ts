import * as WS from "./WebsocketTypes"
import * as T from "./Types"
import { Slice, Node, Mark, Fragment } from "prosemirror-model";
import * as Editor from "./Editor/Editor"

document.addEventListener("editorAttached", (ev : Editor.EditorAttached) => {
  const editor = ev.editor;
  const container = document.getElementById("studyBlockEditor");
  editor.addEventListener("studyblocks-edit", (ev : Editor.EditorStudyBlocksEdit) => {
    container.innerHTML = "";
    window.toggleModal("#studyBlockEditor");
    const studyBlocks = ev.studyBlocks;
    studyBlocks.forEach( ({node}, _, index) => {
      const div = document.createElement("div");

      div.setAttribute("originalIndex", index + "");
      div.setAttribute("currentIndex", index + "");
      div.setAttribute("oldIndex", index + "");

      let name = "";
      if (node.type.name == "questions") {
        name = "Questions";
      } else {
        node.descendants( (node: Node) => {
          if (node.type.name === "generalStudyBlockHeader") {
            name = node.child(0).text;
          }
          return false;
        })
      }
      div.innerHTML = name;
      container.appendChild(div);
    });
  });
});

