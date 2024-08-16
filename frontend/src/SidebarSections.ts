import { Node } from "prosemirror-model";
import * as Editor from "./Editor/Editor"
import * as EditorUtil from "./Editor/editorUtils"

document.addEventListener("editorAttached", (ev : Editor.EditorAttached) => {
  const editor = ev.editor;
  const container = document.getElementById("sectionContainer");
  const doc = editor.view.state.doc;
  initialSetup(editor, container, doc);
  editor.onUpdate( (doc) => {
    if (doc.childCount > container.children.length) {
      for (let i = 0; i < doc.childCount - container.children.length; i++) {
        container.appendChild(createSectionHeader(editor, container.children.length+i));
      }
    } else if (doc.childCount < container.children.length) {
      for (let i = 0; i < container.children.length - doc.childCount; i++) {
        container.children.item(container.children.length-1-i).remove();
      }
    }
    updateSectionNames(container, doc);
  });
});


function updateSectionNames (container: HTMLElement, doc : Node) {
  doc.forEach( (node, _, index) => {
    const sectionHeader = node.child(0);
    const sectionText = sectionHeader.child(0);
    const oldTxt = container.children.item(index).innerHTML;
    if (oldTxt != sectionText.text) {
      container.children.item(index).querySelector("span").innerHTML = sectionText.text;
    }
  });
}

function initialSetup(editor : Editor.P215Editor, container: HTMLElement, doc : Node) {
  doc.forEach( (node, _, index) => {
    const section = createSectionHeader(editor, index);
    section.querySelector("span").innerHTML = node.child(0).child(0).text;
    container.appendChild(section);
  });
}

function createSectionHeader(editor : Editor.P215Editor, index : number) {
  const section = document.createElement("div");
  section.setAttribute("currentIndex", index + "");
  section.setAttribute("oldIndex", index + "");

  section.className = "section";
  section.addEventListener("click", () => {
    const currentIndex = section.getAttribute("currentIndex")
    editor.scrollTo(Number(currentIndex));
  });

  const sectionText = document.createElement("span");
  section.appendChild(sectionText);

  const remove = document.createElement("div");
  remove.innerHTML = "x";
  const img = document.createElement("img");
  img.src = "/static/img/x.svg";
  remove.className = "remove";
  remove.addEventListener("click", (e) => {
    e.stopPropagation();

    const currentIndex = section.getAttribute("currentIndex")
    editor.applyDispatch(EditorUtil.deleteSection(Number(currentIndex)));
  });
  section.appendChild(remove);

  function moveDown () {
    const next = section.nextElementSibling;
    if(next) {
      let ncurrentIndex = Number(next.getAttribute("currentIndex"));
      ncurrentIndex --;
      next.setAttribute("currentIndex", ncurrentIndex + "");
      next.setAttribute("oldIndex", ncurrentIndex + "");

      section.parentElement.insertBefore(next, section);
      let currentIndex = Number(section.getAttribute("currentIndex"));
      currentIndex ++;
      section.setAttribute("currentIndex", currentIndex + "");
    }
  }

  function moveUp () {
    const previous = section.previousElementSibling;
    if(previous) {
      let pcurrentIndex = Number(previous.getAttribute("currentIndex"));
      pcurrentIndex ++;
      previous.setAttribute("currentIndex", pcurrentIndex + "");
      previous.setAttribute("oldIndex", pcurrentIndex + "");

      section.parentElement.insertBefore(section, previous);
      let currentIndex = Number(section.getAttribute("currentIndex"));
      currentIndex --;
      section.setAttribute("currentIndex", currentIndex + "");
    }
  }

  function mousemove (e : MouseEvent) {
    section.classList.add("moving");
    const rec = section.getBoundingClientRect();
    if(e.clientY > rec.bottom) {
      moveDown();
    } else if (e.clientY < rec.top) {
      moveUp();
    }
  };

  function mouseup () {
    document.removeEventListener("mouseup", mouseup);
    document.removeEventListener("mousemove", mousemove);
    section.classList.remove("moving");
    let currentIndex = Number(section.getAttribute("currentIndex"));
    let oldIndex = Number(section.getAttribute("oldIndex"));
    if (currentIndex != oldIndex) {
      editor.applyDispatch(EditorUtil.moveSection(oldIndex, currentIndex));
      section.setAttribute("oldIndex", currentIndex + "");
    }
  };

  section.addEventListener("mousedown", (e: MouseEvent) => {
    if(e.button == 0) {
      document.addEventListener("mousemove", mousemove);
      document.addEventListener("mouseup", mouseup);
    }
  });

  section.addEventListener("touchstart", (e : TouchEvent) => {
    e.preventDefault();
    let isMoving = false;
    let isWipeRight = false;
    let initialTouch = copyTouch(e.touches[0]);
    let moveTimeout = setTimeout(() => {
      if(e.touches.length == 1) {
        section.classList.add("moving");
        isMoving = true;
      }
    }, 400);

    function touchMove(e2 : TouchEvent) {
      clearTimeout(moveTimeout);
      if(isMoving) {
        const rec = section.getBoundingClientRect();
        const touch = e2.touches[0];
        if (touch.clientY > rec.bottom) {
          moveDown();
        } else if (touch.clientY < rec.top) {
          moveUp();
        }
      } else {
      }
    }

    function touchEnd() {
      clearTimeout(moveTimeout);
      document.removeEventListener("touchmove", touchMove);
      document.removeEventListener("touchend", touchEnd);
      if(isMoving) {
        section.classList.remove("moving");
        let currentIndex = Number(section.getAttribute("currentIndex"));
        let oldIndex = Number(section.getAttribute("oldIndex"));
        if (currentIndex != oldIndex) {
          editor.applyDispatch(EditorUtil.moveSection(oldIndex, currentIndex));
          section.setAttribute("oldIndex", currentIndex + "");
        }
      } else {
        const currentIndex = section.getAttribute("currentIndex")
        editor.scrollTo(Number(currentIndex));
      }
    }

    document.addEventListener("touchmove", touchMove);
    document.addEventListener("touchend", touchEnd);

  });
  return section;
};

function copyTouch (touch : Touch) {
  return {
    clientX: touch.clientX,
    clientY: touch.clientY,
  }
}
