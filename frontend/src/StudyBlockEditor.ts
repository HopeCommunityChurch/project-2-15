import * as Editor from "./Editor/Editor"
import * as EditorUtil from "./Editor/editorUtils"
import { Node, DOMSerializer } from "prosemirror-model"
import { textSchema } from "./Editor/textSchema"

document.addEventListener("editorAttached", (ev : Editor.EditorAttached) => {
  const editor = ev.editor;
  const container = document.getElementById("studyBlockEditorContent");
  let currentStudyBlock : Editor.EditorStudyBlocksEdit = null;

  document.getElementById("studyBlockCancel").addEventListener("click", () => {
    window.toggleModal("#studyBlockEditor");
  });

  const removedSection = document.getElementById("studyBlockEditorRemoved");
  const removedList = document.getElementById("studyBlockEditorRemovedList");

  // Get or create the "Add block" button below the content area
  let addBlockBtn = document.getElementById("studyBlockAddBtn") as HTMLButtonElement | null;
  if (!addBlockBtn) {
    addBlockBtn = document.createElement("button");
    addBlockBtn.id = "studyBlockAddBtn";
    addBlockBtn.className = "addBlockBtn";
    addBlockBtn.textContent = "+ Add block";
    container.parentElement.insertBefore(addBlockBtn, container.nextSibling);
  }

  const bodySerializer = DOMSerializer.fromSchema(textSchema);

  function populateBodyPreview(node: Node, el: HTMLElement): void {
    node.forEach(child => {
      if (child.type.name === "generalStudyBlockBody") {
        // Only render the first 2 paragraphs so no third line ever enters the DOM
        let count = 0;
        child.forEach(para => {
          if (count >= 2) return;
          const serialized = bodySerializer.serializeNode(para);
          el.appendChild(serialized);
          count++;
        });
      }
    });
  }

  function makeBlockItem(node: Node | null, index: number | null, isNew: boolean): HTMLElement {
    const div = document.createElement("div");
    div.className = "studyBlock";

    const handle = document.createElement("img");
    handle.src = "/static/img/drag-handle.svg";
    handle.className = "dragHandle";
    handle.draggable = false;
    div.appendChild(handle);

    let startMouseY = 0;
    let initialTop = 0;
    let swapping = false;
    let placeholder: HTMLElement = null;

    div.addEventListener("mousedown", (e: MouseEvent) => {
      if (e.button === 0) {
        // Don't start drag if clicking on the input
        if ((e.target as HTMLElement).tagName === "INPUT") return;
        e.preventDefault();
        startMouseY = e.clientY;
        document.addEventListener("mousemove", mousemove);
        document.addEventListener("mouseup", mouseup);
      }
    });

    function mouseup() {
      document.removeEventListener("mouseup", mouseup);
      document.removeEventListener("mousemove", mousemove);
      if (placeholder) {
        placeholder.remove();
        placeholder = null;
      }
      div.classList.remove("moving");
      div.style.top = "";
    }

    function mousemove(e: MouseEvent) {
      if (!div.classList.contains("moving")) {
        if (Math.abs(e.clientY - startMouseY) < 3) return;
        const rect = div.getBoundingClientRect();
        const containerRect = div.parentElement.getBoundingClientRect();
        initialTop = rect.top - containerRect.top;
        div.classList.add("moving");
        placeholder = document.createElement("div");
        placeholder.className = "dropPlaceholder";
        placeholder.style.height = rect.height + "px";
        div.parentElement.insertBefore(placeholder, div);
        div.style.top = initialTop + "px";
      }

      const dy = e.clientY - startMouseY;
      div.style.top = (initialTop + dy) + "px";

      if (swapping) return;

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
    }

    function doSwap(sibling: HTMLElement, direction: "up" | "down") {
      swapping = true;
      if (direction === "down") {
        div.parentElement.insertBefore(sibling, placeholder);
      } else {
        div.parentElement.insertBefore(placeholder, sibling);
        div.parentElement.insertBefore(div, sibling);
      }
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
          setTimeout(() => { swapping = false; }, 250);
        });
      });
    }

    if (isNew) {
      div.setAttribute("data-new", "true");
    } else {
      div.setAttribute("originalIndex", index + "");
    }

    // Build preview row
    const preview = document.createElement("div");
    preview.className = "blockPreview";

    const headerCell = document.createElement("div");
    headerCell.className = "blockPreviewHeader";

    const isQuestions = node && node.type.name === "questions";

    if (isQuestions) {
      headerCell.textContent = "Questions";
    } else {
      const nameInput = document.createElement("textarea");
      nameInput.rows = 1;
      // Prevent Enter from inserting newlines — block names are single-line
      nameInput.addEventListener("keydown", (e: KeyboardEvent) => {
        if (e.key === "Enter") e.preventDefault();
      });
      // Grow the textarea vertically as text wraps
      const resizeTextarea = () => {
        nameInput.style.height = "auto";
        nameInput.style.height = nameInput.scrollHeight + "px";
      };
      nameInput.addEventListener("input", resizeTextarea);
      // Resize on initial load once the element is in the DOM
      requestAnimationFrame(resizeTextarea);
      // Clicking anywhere in the header cell focuses the textarea
      headerCell.addEventListener("click", () => nameInput.focus());
      nameInput.className = "blockName";
      if (node) {
        let name = "";
        node.forEach(child => {
          if (child.type.name === "generalStudyBlockHeader") {
            name = child.textContent;
          }
        });
        nameInput.value = name;
        nameInput.placeholder = "Untitled";
      } else {
        nameInput.value = "";
        nameInput.placeholder = "Block name";
      }
      headerCell.appendChild(nameInput);
      // Focus input for new blocks
      if (isNew) {
        requestAnimationFrame(() => nameInput.focus());
      }
    }

    const bodyCell = document.createElement("div");
    bodyCell.className = "blockPreviewBody";
    if (!isQuestions && node) {
      populateBodyPreview(node, bodyCell);
    }

    preview.appendChild(headerCell);
    preview.appendChild(bodyCell);
    div.appendChild(preview);

    if (!isQuestions) {
      const remove = document.createElement("button");
      remove.className = "remove";
      const removeImg = document.createElement("img");
      removeImg.src = "/static/img/x.svg";
      removeImg.draggable = false;
      removeImg.alt = "";
      remove.appendChild(removeImg);
      remove.addEventListener("click", (e) => {
        e.stopPropagation();
        // For new blocks, just remove entirely — no restore
        if (isNew) {
          container.removeChild(div);
          return;
        }
        container.removeChild(div);
        const name = (headerCell.querySelector("input") as HTMLInputElement)?.value
          || (node ? node.textContent : "");
        const removedDiv = document.createElement("div");
        removedDiv.className = "studyBlock";
        const removedName = document.createElement("div");
        removedName.innerText = name || "Untitled";
        removedName.style.textDecoration = "line-through";
        removedDiv.appendChild(removedName);
        const restoreBtn = document.createElement("button");
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

    return div;
  }

  editor.addEventListener("studyblocks-edit", (ev : Editor.EditorStudyBlocksEdit) => {
    container.innerHTML = "";
    removedList.innerHTML = "";
    removedSection.style.display = "none";
    window.toggleModal("#studyBlockEditor");
    const studyBlocks = ev.studyBlocks;
    currentStudyBlock = ev;

    studyBlocks.forEach(({node}, index) => {
      container.appendChild(makeBlockItem(node, index, false));
    });
  });

  addBlockBtn.addEventListener("click", () => {
    if (currentStudyBlock === null) return;
    container.appendChild(makeBlockItem(null, null, true));
  });

  document.getElementById("studyBlockUpdate").addEventListener("click", () => {
    if (currentStudyBlock === null) return;
    const newNodes: Node[] = [];
    const children = container.children;
    for (let i = 0; i < children.length; i++) {
      const child = children[i] as HTMLElement;
      if (child.getAttribute("data-new") === "true") {
        const input = child.querySelector("textarea.blockName") as HTMLTextAreaElement;
        const name = (input?.value.trim()) || "Untitled";
        newNodes.push(EditorUtil.createGeneralStudyBlockNode(name));
      } else {
        const originalIndex = Number(child.getAttribute("originalIndex"));
        let node = currentStudyBlock.studyBlocks[originalIndex].node;
        if (node.type.name !== "questions") {
          const input = child.querySelector("textarea.blockName") as HTMLTextAreaElement;
          const newName = (input?.value.trim()) || "Untitled";
          node = EditorUtil.withRenamedHeader(node, newName);
        }
        newNodes.push(node);
      }
    }
    editor.applyDispatch(EditorUtil.reorderStudyBlock(
      currentStudyBlock.studyBlockPos,
      newNodes
    ));
    window.toggleModal("#studyBlockEditor");
  });
});
