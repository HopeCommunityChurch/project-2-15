import { Node } from "prosemirror-model";
import * as Editor from "./Editor/Editor"
import * as EditorUtil from "./Editor/editorUtils"

document.addEventListener("editorAttached", (ev : Editor.EditorAttached) => {
  const editor = ev.editor;
  const container = document.getElementById("studyBlockEditorContent");
  let currentStudyBlock : Editor.EditorStudyBlocksEdit = null;

  document.getElementById("studyBlockCancel").addEventListener("click", () => {
    window.toggleModal("#studyBlockEditor");
  });

  editor.addEventListener("studyblocks-edit", (ev : Editor.EditorStudyBlocksEdit) => {
    container.innerHTML = "";
    window.toggleModal("#studyBlockEditor");
    const studyBlocks = ev.studyBlocks;
    currentStudyBlock = ev;

    studyBlocks.forEach( ({node}, index) => {
      const div = document.createElement("div");
      div.className = "studyBlock";

      div.addEventListener("mousedown", (e: MouseEvent) => {
        if(e.button == 0) {
          document.addEventListener("mousemove", mousemove);
          document.addEventListener("mouseup", mouseup);
        }
      });


      function mouseup () {
        document.removeEventListener("mouseup", mouseup);
        document.removeEventListener("mousemove", mousemove);
        div.classList.remove("moving");
        let currentIndex = Number(div.getAttribute("currentIndex"));
      };

      function mousemove (e : MouseEvent) {
        div.classList.add("moving");
        const rec = div.getBoundingClientRect();
        if(e.clientY > rec.bottom) {
          moveDown();
        } else if (e.clientY < rec.top) {
          moveUp();
        }
      };

      function moveDown () {
        const next = div.nextElementSibling;
        if(next) {
          let ncurrentIndex = Number(next.getAttribute("currentIndex"));
          ncurrentIndex --;
          next.setAttribute("currentIndex", ncurrentIndex + "");

          div.parentElement.insertBefore(next, div);
          let currentIndex = Number(div.getAttribute("currentIndex"));
          currentIndex ++;
          div.setAttribute("currentIndex", currentIndex + "");
        }
      }

      function moveUp () {
        const previous = div.previousElementSibling;
        if(previous) {
          let pcurrentIndex = Number(previous.getAttribute("currentIndex"));
          pcurrentIndex ++;
          previous.setAttribute("currentIndex", pcurrentIndex + "");

          div.parentElement.insertBefore(div, previous);
          let currentIndex = Number(div.getAttribute("currentIndex"));
          currentIndex --;
          div.setAttribute("currentIndex", currentIndex + "");
        }
      }




      div.setAttribute("originalIndex", index + "");
      div.setAttribute("currentIndex", index + "");

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
      const div2 = document.createElement("div");
      div2.innerText = name;
      div.appendChild(div2);

      if (node.type.name != "questions") {
        const remove = document.createElement("button");
        remove.type = "button";
        remove.innerText = "х";
        remove.className = "remove"
        remove.addEventListener("click", (e) => {
          container.removeChild(div);
        });
        div.appendChild(remove);
      }

      container.appendChild(div);
    });
  });

  document.getElementById("studyBlockUpdate").addEventListener("click", () => {
    const container = document.getElementById("studyBlockEditorContent");
    if (currentStudyBlock === null) return;
    let newPosistions = [];
    const children = container.children
    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      const currentIndex = Number(child.getAttribute("currentIndex"));
      const originalIndex = Number(child.getAttribute("originalIndex"));
      newPosistions[currentIndex] = currentStudyBlock.studyBlocks[originalIndex].node;
    }
    editor.applyDispatch(EditorUtil.reorderStudyBlock(
      currentStudyBlock.studyBlockPos,
      newPosistions
    ));
    window.toggleModal("#studyBlockEditor");
  });
});


