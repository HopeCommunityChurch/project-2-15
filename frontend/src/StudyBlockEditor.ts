import * as Editor from "./Editor/Editor"
import * as EditorUtil from "./Editor/editorUtils"

document.addEventListener("editorAttached", (ev : Editor.EditorAttached) => {
  const editor = ev.editor;
  const container = document.getElementById("studyBlockEditorContent");
  let currentStudyBlock : Editor.EditorStudyBlocksEdit = null;

  document.getElementById("studyBlockCancel").addEventListener("click", () => {
    window.toggleModal("#studyBlockEditor");
  });

  const removedSection = document.getElementById("studyBlockEditorRemoved");
  const removedList = document.getElementById("studyBlockEditorRemovedList");

  editor.addEventListener("studyblocks-edit", (ev : Editor.EditorStudyBlocksEdit) => {
    container.innerHTML = "";
    removedList.innerHTML = "";
    removedSection.style.display = "none";
    window.toggleModal("#studyBlockEditor");
    const studyBlocks = ev.studyBlocks;
    currentStudyBlock = ev;

    studyBlocks.forEach( ({node}, index) => {
      const div = document.createElement("div");
      div.className = "studyBlock";

      const handle = document.createElement("img");
      handle.src = "/static/img/drag-handle.svg";
      handle.className = "dragHandle";
      handle.draggable = false;
      div.appendChild(handle);

      let startMouseY = 0;
      let startMouseX = 0;
      let initialTop = 0;
      let initialLeft = 0;
      let swapping = false;

      let placeholder: HTMLElement = null;

      div.addEventListener("mousedown", (e: MouseEvent) => {
        if(e.button === 0) {
          e.preventDefault();
          startMouseY = e.clientY;
          startMouseX = e.clientX;
          document.addEventListener("mousemove", mousemove);
          document.addEventListener("mouseup", mouseup);
        }
      });

      function mouseup () {
        document.removeEventListener("mouseup", mouseup);
        document.removeEventListener("mousemove", mousemove);

        if (placeholder) {
          const placeholderRect = placeholder.getBoundingClientRect();
          const containerRect = div.parentElement.getBoundingClientRect();
          const targetTop = placeholderRect.top - containerRect.top;
          const targetLeft = placeholderRect.left - containerRect.left;

          div.style.transition = "top 0.2s ease, left 0.2s ease, transform 0.2s ease, box-shadow 0.2s ease";
          div.style.top = targetTop + "px";
          div.style.left = targetLeft + "px";
          div.style.transform = "";

          div.addEventListener("transitionend", function settle() {
            div.removeEventListener("transitionend", settle);
            div.style.top = "";
            div.style.left = "";
            if (placeholder) {
              placeholder.remove();
              placeholder = null;
            }
            // Fade out the blue background slowly
            div.style.transition = "background-color 0.5s ease, box-shadow 0.3s ease";
            div.classList.remove("moving");
            div.addEventListener("transitionend", function fadeout() {
              div.removeEventListener("transitionend", fadeout);
              div.style.transition = "";
            }, { once: true });
          }, { once: true });

          // Fallback in case transitionend doesn't fire
          setTimeout(() => {
            div.style.top = "";
            div.style.left = "";
            div.style.transition = "background-color 0.5s ease, box-shadow 0.3s ease";
            div.classList.remove("moving");
            if (placeholder) {
              placeholder.remove();
              placeholder = null;
            }
            setTimeout(() => { div.style.transition = ""; }, 600);
          }, 300);
        } else {
          div.classList.remove("moving");
          div.style.transform = "";
          div.style.top = "";
          div.style.left = "";
        }
      };

      function mousemove (e : MouseEvent) {
        if (!div.classList.contains("moving")) {
          if (Math.abs(e.clientY - startMouseY) < 3) return;
          const rect = div.getBoundingClientRect();
          const containerRect = div.parentElement.getBoundingClientRect();
          initialTop = rect.top - containerRect.top;
          initialLeft = rect.left - containerRect.left;
          div.classList.add("moving");
          placeholder = document.createElement("div");
          placeholder.className = "dropPlaceholder";
          placeholder.style.height = rect.height + "px";
          div.parentElement.insertBefore(placeholder, div);
          div.style.top = initialTop + "px";
          div.style.left = initialLeft + "px";
        }

        const dy = e.clientY - startMouseY;
        const dx = e.clientX - startMouseX;
        div.style.top = (initialTop + dy) + "px";
        div.style.left = (initialLeft + dx) + "px";
        div.style.transform = `scale(0.96) rotate(1.5deg)`;

        if (swapping) return;

        // Use the cursor position for hit testing
        const next = div.nextElementSibling as HTMLElement;
        const previous = placeholder ? placeholder.previousElementSibling as HTMLElement : null;

        if (next && !next.classList.contains("dropPlaceholder")) {
          const nextRect = next.getBoundingClientRect();
          const nextMid = nextRect.top + nextRect.height * 0.2;
          if (e.clientY > nextMid) {
            doSwap(next, "down");
            return;
          }
        }
        if (previous && !previous.classList.contains("dropPlaceholder")) {
          const prevRect = previous.getBoundingClientRect();
          const prevMid = prevRect.top + prevRect.height * 0.8;
          if (e.clientY < prevMid) {
            doSwap(previous, "up");
          }
        }
      };

      function doSwap(sibling: HTMLElement, direction: "up" | "down") {
        swapping = true;
        // Update indices
        let sIndex = Number(sibling.getAttribute("currentIndex"));
        let dIndex = Number(div.getAttribute("currentIndex"));
        if (direction === "down") {
          sIndex--; dIndex++;
        } else {
          sIndex++; dIndex--;
        }
        sibling.setAttribute("currentIndex", sIndex + "");
        div.setAttribute("currentIndex", dIndex + "");

        // Move the placeholder (div stays absolute, unaffected by DOM order)
        if (direction === "down") {
          div.parentElement.insertBefore(sibling, placeholder);
        } else {
          div.parentElement.insertBefore(placeholder, sibling);
          div.parentElement.insertBefore(div, sibling);
        }

        // Animate the displaced sibling from its old position
        const placeholderHeight = placeholder.getBoundingClientRect().height;
        const fromOffset = direction === "down" ? placeholderHeight : -placeholderHeight;
        sibling.classList.add("swapping");
        sibling.style.transform = `translateY(${fromOffset}px)`;
        requestAnimationFrame(() => {
          requestAnimationFrame(() => {
            sibling.style.transform = "";
            sibling.addEventListener("transitionend", () => {
              sibling.classList.remove("swapping");
              swapping = false;
            }, { once: true });
            // Fallback in case transitionend doesn't fire
            setTimeout(() => { swapping = false; }, 250);
          });
        });
      }

      div.setAttribute("originalIndex", index + "");
      div.setAttribute("currentIndex", index + "");

      let name = "";
      if (node.type.name === "questions") {
        name = "Questions";
      } else {
        node.descendants( (node) => {
          if (node.type.name === "generalStudyBlockHeader") {
            name = node.child(0).text;
          }
          return false;
        })
      }
      const div2 = document.createElement("div");
      div2.innerText = name;
      div.appendChild(div2);

      if (node.type.name !== "questions") {
        const remove = document.createElement("img");
        remove.src = "/static/img/x.svg";
        remove.className = "remove";
        remove.draggable = false;
        remove.addEventListener("click", (e) => {
          e.stopPropagation();
          container.removeChild(div);
          const removedDiv = document.createElement("div");
          removedDiv.className = "studyBlock";
          const removedName = document.createElement("div");
          removedName.innerText = name;
          removedName.style.textDecoration = "line-through";
          removedDiv.appendChild(removedName);
          const restoreBtn = document.createElement("div");
          restoreBtn.className = "restore";
          const restoreIcon = document.createElement("img");
          restoreIcon.src = "/static/img/undo-icon.svg";
          restoreIcon.draggable = false;
          restoreBtn.appendChild(restoreIcon);
          const restoreText = document.createElement("span");
          restoreText.innerText = "Restore";
          restoreBtn.appendChild(restoreText);
          restoreBtn.addEventListener("click", () => {
            removedList.removeChild(removedDiv);
            if (removedList.children.length === 0) {
              removedSection.style.display = "none";
            }
            container.appendChild(div);
          });
          removedDiv.appendChild(restoreBtn);
          removedList.appendChild(removedDiv);
          removedSection.style.display = "block";
        });
        div.appendChild(remove);
      }

      container.appendChild(div);
    });
  });

  document.getElementById("studyBlockUpdate").addEventListener("click", () => {
    if (currentStudyBlock === null) return;
    let newPositions = [];
    const children = container.children
    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      const currentIndex = Number(child.getAttribute("currentIndex"));
      const originalIndex = Number(child.getAttribute("originalIndex"));
      newPositions[currentIndex] = currentStudyBlock.studyBlocks[originalIndex].node;
    }
    editor.applyDispatch(EditorUtil.reorderStudyBlock(
      currentStudyBlock.studyBlockPos,
      newPositions
    ));
    window.toggleModal("#studyBlockEditor");
  });
});
