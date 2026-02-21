import * as Editor from "./Editor/Editor"
import * as EditorUtil from "./Editor/editorUtils"
import { Node, DOMSerializer } from "prosemirror-model"
import { textSchema } from "./Editor/textSchema"

document.addEventListener("editorAttached", (ev : Editor.EditorAttached) => {
  const editor = ev.editor;
  const container = document.getElementById("studyBlockEditorContent");
  let currentStudyBlock : Editor.EditorStudyBlocksEdit = null;

  // Track any in-progress drag so it can be aborted when the modal closes
  let activeDragAbort: (() => void) | null = null;

  function closeModal() {
    if (activeDragAbort) {
      activeDragAbort();
      activeDragAbort = null;
    }
    window.toggleModal("#studyBlockEditor");
  }

  document.getElementById("studyBlockCancel").addEventListener("click", closeModal);

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

    const isQuestions = node && node.type.name === "questions";

    // removeBtn/restoreBtn declared here so mouseup closure can access them
    let removeBtn: HTMLButtonElement = null;
    let restoreBtn: HTMLButtonElement = null;

    let startMouseY = 0;
    let startFixedTop = 0;
    let placeholder: HTMLElement = null;
    // Captured at drag-start so mouseup doesn't rely on div.parentElement after moves
    let dragStartedInRemovedList = false;

    div.addEventListener("mousedown", (e: MouseEvent) => {
      if (e.button === 0) {
        // Don't start drag if clicking on the input or textarea
        const tag = (e.target as HTMLElement).tagName;
        if (tag === "INPUT" || tag === "TEXTAREA") return;
        e.preventDefault();
        startMouseY = e.clientY;
        document.addEventListener("mousemove", mousemove);
        document.addEventListener("mouseup", mouseup);
        activeDragAbort = abortDrag;
      }
    });

    function abortDrag() {
      document.removeEventListener("mouseup", mouseup);
      document.removeEventListener("mousemove", mousemove);
      if (placeholder) {
        placeholder.remove();
        placeholder = null;
      }
      div.classList.remove("moving");
      div.style.position = "";
      div.style.top = "";
      div.style.left = "";
      div.style.width = "";
    }

    function mouseup() {
      activeDragAbort = null;
      document.removeEventListener("mouseup", mouseup);
      document.removeEventListener("mousemove", mousemove);
      if (placeholder) {
        const targetList = placeholder.parentElement;
        targetList.insertBefore(div, placeholder);
        placeholder.remove();
        placeholder = null;

        // Clear fixed positioning
        div.style.position = "";
        div.style.top = "";
        div.style.left = "";
        div.style.width = "";
        div.classList.remove("moving");

        // Swap remove/restore buttons if div crossed lists
        if (removeBtn && restoreBtn) {
          const isNowInRemovedList = targetList === removedList;
          if (isNowInRemovedList && !dragStartedInRemovedList) {
            removeBtn.style.display = "none";
            restoreBtn.style.display = "";
          } else if (!isNowInRemovedList && dragStartedInRemovedList) {
            removeBtn.style.display = "";
            restoreBtn.style.display = "none";
          }
        }

        // Sync removedSection visibility
        removedSection.style.display = removedList.children.length > 0 ? "block" : "none";
      } else {
        div.classList.remove("moving");
        div.style.position = "";
        div.style.top = "";
        div.style.left = "";
        div.style.width = "";
      }
    }

    function mousemove(e: MouseEvent) {
      if (!div.classList.contains("moving")) {
        if (Math.abs(e.clientY - startMouseY) < 3) return;
        const rect = div.getBoundingClientRect();
        dragStartedInRemovedList = div.parentElement === removedList;
        // Adjust startFixedTop so the element doesn't jump when drag begins.
        // Without this, the initial dy would offset the element by the threshold distance.
        startFixedTop = rect.top - (e.clientY - startMouseY);
        div.classList.add("moving");
        placeholder = document.createElement("div");
        placeholder.className = "dropPlaceholder";
        placeholder.style.height = rect.height + "px";
        div.parentElement.insertBefore(placeholder, div);
        div.style.position = "fixed";
        div.style.top = (startFixedTop + (e.clientY - startMouseY)) + "px";
        div.style.left = rect.left + "px";
        div.style.width = rect.width + "px";
      }

      const dy = e.clientY - startMouseY;
      div.style.top = (startFixedTop + dy) + "px";

      // Determine target list with hysteresis to prevent oscillation.
      // When placeholder is in removedList the section shifts up, so we only
      // switch back to container when the mouse is clearly above the new top.
      let targetList: HTMLElement;
      if (!isQuestions && removedSection.style.display !== "none") {
        const sectionTop = removedSection.getBoundingClientRect().top;
        if (placeholder.parentElement !== removedList) {
          targetList = e.clientY >= sectionTop ? removedList : container;
        } else {
          targetList = e.clientY < sectionTop - 10 ? container : removedList;
        }
      } else {
        targetList = container;
      }

      // Find insertion point in target list
      const children = Array.from(targetList.children) as HTMLElement[];
      let insertBefore: Element | null = null;
      for (const child of children) {
        if (child === placeholder || child === div) continue;
        const childRect = child.getBoundingClientRect();
        const midY = childRect.top + childRect.height / 2;
        if (e.clientY < midY) {
          insertBefore = child;
          break;
        }
      }

      // Move placeholder if position changed
      if (placeholder.parentElement !== targetList) {
        targetList.insertBefore(placeholder, insertBefore);
      } else if (placeholder.nextElementSibling !== insertBefore) {
        targetList.insertBefore(placeholder, insertBefore);
      }
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
        nameInput.placeholder = "Block title";
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
      removeBtn = document.createElement("button");
      removeBtn.className = "remove";
      const removeImg = document.createElement("img");
      removeImg.src = "/static/img/x.svg";
      removeImg.draggable = false;
      removeImg.alt = "";
      removeBtn.appendChild(removeImg);

      restoreBtn = document.createElement("button");
      restoreBtn.className = "remove";
      const restoreImg = document.createElement("img");
      restoreImg.src = "/static/img/undo-icon.svg";
      restoreImg.draggable = false;
      restoreImg.alt = "Restore";
      restoreBtn.appendChild(restoreImg);
      restoreBtn.style.display = "none";

      removeBtn.addEventListener("mousedown", (e) => e.stopPropagation());
      restoreBtn.addEventListener("mousedown", (e) => e.stopPropagation());

      removeBtn.addEventListener("click", (e) => {
        e.stopPropagation();
        div.parentElement.removeChild(div);
        removeBtn.style.display = "none";
        restoreBtn.style.display = "";
        removedList.appendChild(div);
        removedSection.style.display = "block";
      });

      restoreBtn.addEventListener("click", (e) => {
        e.stopPropagation();
        div.parentElement.removeChild(div);
        restoreBtn.style.display = "none";
        removeBtn.style.display = "";
        container.appendChild(div);
        if (removedList.children.length === 0) {
          removedSection.style.display = "none";
        }
      });

      div.appendChild(removeBtn);
      div.appendChild(restoreBtn);
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
