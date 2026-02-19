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
    const sectionText = sectionHeader.maybeChild(0);
    const txt = (sectionText)? sectionText.text : "Untitled";
    const oldTxt = container.children.item(index).innerHTML;
    if (oldTxt != txt) {
      container.children.item(index).querySelector("span").innerText = txt;
    }
  });
}

function initialSetup(editor : Editor.P215Editor, container: HTMLElement, doc : Node) {
  doc.forEach( (node, _, index) => {
    const section = createSectionHeader(editor, index);
    const sectionHeader = node.child(0);
    const sectionText = sectionHeader.maybeChild(0);
    const txt = (sectionText)? sectionText.text : "Untitled";
    section.querySelector("span").innerText = txt;
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
  const img = document.createElement("img");
  img.src = "/static/img/x.svg";
  remove.appendChild(img);
  remove.className = "remove";
  remove.addEventListener("click", (e) => {
    e.stopPropagation();

    const currentIndex = section.getAttribute("currentIndex")
    editor.applyDispatch(EditorUtil.deleteSection(Number(currentIndex)));
  });
  section.appendChild(remove);

  const deleter = document.createElement("div");
  deleter.className = "deleter";
  deleter.innerText = "Remove";
  section.appendChild(deleter);
  deleter.addEventListener("touchstart", (e) => {
    e.stopPropagation();
    closeAllDeleters(section);
    const currentIndex = section.getAttribute("currentIndex")
    editor.applyDispatch(EditorUtil.deleteSection(Number(currentIndex)));
  });

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
    closeAllDeleters(section);
    const startTime = e.timeStamp;
    let state : TouchStates = {
      swipeState: {
        isNotSwipe: false,
        initialTouch: copyTouch(e.touches[0]),
      },
      currentGesture: "None",
    }

    let moveTimeout = setTimeout(() => {
      if(e.touches.length == 1) {
        section.classList.add("moving");
        state.currentGesture = "Moving";
      }
    }, 400);

    function touchMove(e2 : TouchEvent) {
      clearTimeout(moveTimeout);
      switch (state.currentGesture) {
        case "Moving": {
          const rec = section.getBoundingClientRect();
          const touch = e2.touches[0];
          if (touch.clientY > rec.bottom) {
            moveDown();
          } else if (touch.clientY < rec.top) {
            moveUp();
          }
          break;
        }
        case "Swipe": {
          const touch = e2.touches[0];
          let moveX = touch.clientX - state.swipeState.initialTouch.clientX;
          section.style.transform = `translateX(${moveX}px)`;
          state.swipeState.lastTouch = copyTouch(touch);
          break;
        }
        case "None": {
          if(!state.swipeState.isNotSwipe) {
            if(e2.touches.length == 1) {
              const touch = e2.touches[0];
              let dX = touch.clientX - state.swipeState.initialTouch.clientX;
              let dY = touch.clientY - state.swipeState.initialTouch.clientY;
              if(Math.abs(dY) > 40) {
                state.swipeState.isNotSwipe = true;
              } else {
                if(dX < -30) {
                  state.currentGesture = "Swipe";
                }
              }
            }
          }
        }
      }

    }

    function touchEnd(e2: TouchEvent) {
      clearTimeout(moveTimeout);
      document.removeEventListener("touchmove", touchMove);
      document.removeEventListener("touchend", touchEnd);
      switch (state.currentGesture) {
        case "Moving": {
          section.classList.remove("moving");
          let currentIndex = Number(section.getAttribute("currentIndex"));
          let oldIndex = Number(section.getAttribute("oldIndex"));
          if (currentIndex != oldIndex) {
            editor.applyDispatch(EditorUtil.moveSection(oldIndex, currentIndex));
            section.setAttribute("oldIndex", currentIndex + "");
          }
          break;
        }
        case "Swipe": {
          const touch = state.swipeState.lastTouch;
          if (touch.clientX - state.swipeState.initialTouch.clientX < -30) {
            section.style.transform = "";
            section.classList.add("swipeOpen");
          } else {
            section.style.transform = "";
          }
          break;
        }
        case "None": {
          const endTime = e2.timeStamp;
          // A tap
          if(endTime - startTime < 100) {
            const currentIndex = section.getAttribute("currentIndex")
            editor.scrollTo(Number(currentIndex));
          }
          break;
        }
      }
    }

    document.addEventListener("touchmove", touchMove);
    document.addEventListener("touchend", touchEnd);
    document.addEventListener("touchcancel", touchEnd);

  });
  return section;
};

type TouchStateSwipe = {
  isNotSwipe: Boolean,
  initialTouch: InitialTouch,
  lastTouch?: InitialTouch,
};


type TouchStates = {
  swipeState: TouchStateSwipe,
  currentGesture: "None" | "Swipe" | "Moving",
};


type InitialTouch = {
  clientX: number,
  clientY: number,
}

function copyTouch (touch : Touch) : InitialTouch {
  return {
    clientX: touch.clientX,
    clientY: touch.clientY,
  }
}

function closeAllDeleters (section: Element) {
  const children = section.parentElement.children
  for(let i = 0; i<children.length; i++) {
    children[i].classList.remove("swipeOpen");
  }
}
