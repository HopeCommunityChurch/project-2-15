import * as WS from "./WebsocketTypes"
import * as T from "./Types"
import * as Editor from "./Editor/Editor"

declare global {
  interface Window {
    pageDocId?: string;
  }
}

type TabEntry = {
  docId: T.DocId;
  label: string;
  tabEl: HTMLElement;
  editorContainerEl: HTMLElement;
  editor: Editor.P215Editor | null;
  isLoading: boolean;
};

let tabs: TabEntry[] = [];
let activeDocId: T.DocId | undefined;
let listenQueue: T.DocId[] = [];
let allDocs: T.DocMetaRaw[] = [];
let labelMap: Map<T.DocId, string> = new Map();

// ── Name disambiguation ──────────────────────────────────────────────

function computeLabels(docs: T.DocMetaRaw[]): Map<T.DocId, string> {
  const map = new Map<T.DocId, string>();
  if (docs.length === 0) return map;

  // Parse names
  const parsed = docs.map(doc => {
    const parts = doc.editors[0]?.name?.split(" ") ?? ["?"];
    const firstName = parts[0] ?? "?";
    const lastName = parts.slice(1).join(" ");
    const lastInitial = lastName ? lastName[0] : "";
    return { doc, firstName, lastName, lastInitial };
  });

  // Group by firstName
  const byFirst = groupBy(parsed, p => p.firstName);

  for (const [firstName, group] of byFirst.entries()) {
    if (group.length === 1) {
      map.set(group[0].doc.docId, firstName);
      continue;
    }
    // Duplicate first names — try firstName + lastInitial
    const byFirstAndInitial = groupBy(group, p => firstName + p.lastInitial);
    for (const [key, subGroup] of byFirstAndInitial.entries()) {
      if (subGroup.length === 1) {
        const p = subGroup[0];
        map.set(p.doc.docId, p.lastInitial ? `${firstName} ${p.lastInitial}` : firstName);
        continue;
      }
      // Duplicate firstName+lastInitial — try firstInitial + lastName
      for (const p of subGroup) {
        const firstInitial = firstName[0] ?? "?";
        if (p.lastName) {
          map.set(p.doc.docId, `${firstInitial}. ${p.lastName}`);
        } else {
          // No last name at all — fall back to firstName + lastInitial
          map.set(p.doc.docId, p.lastInitial ? `${firstName} ${p.lastInitial}` : firstName);
        }
      }
    }
  }

  return map;
}

function groupBy<T>(arr: T[], key: (item: T) => string): Map<string, T[]> {
  const map = new Map<string, T[]>();
  for (const item of arr) {
    const k = key(item);
    const existing = map.get(k);
    if (existing) {
      existing.push(item);
    } else {
      map.set(k, [item]);
    }
  }
  return map;
}

// ── Scroll memory ────────────────────────────────────────────────────

function makeScrollSaver(container: HTMLElement, pageDocId: string, docId: T.DocId) {
  const key = `split-scroll:${pageDocId}:${docId}`;
  let timer: ReturnType<typeof setTimeout> | null = null;

  container.addEventListener("scroll", () => {
    if (timer) clearTimeout(timer);
    timer = setTimeout(() => {
      localStorage.setItem(key, container.scrollTop.toString());
    }, 200);
  });

  return {
    restore() {
      const saved = parseInt(localStorage.getItem(key) ?? '', 10);
      requestAnimationFrame(() => {
        container.scrollTop = isNaN(saved) ? 0 : saved;
      });
    },
    save() {
      localStorage.setItem(key, container.scrollTop.toString());
    },
  };
}

// ── Tab drag-to-reorder ──────────────────────────────────────────────

function initTabDrag(pageDocId: string) {
  const tabBar = document.getElementById("tabBar");
  if (!tabBar) return;

  let dragEl: HTMLElement | null = null;
  let dragOffsetX = 0;   // cursor x offset from tab's left edge at drag start
  let naturalLeft = 0;   // tab's left in bar-coords (no transform), kept up to date on reinsert
  let startClientX = 0;
  let isDragging = false;
  let barLeft = 0;

  // After a DOM reinsert, animate siblings from their old positions back to
  // their new natural positions using the FLIP technique.
  function flipSiblings(snapshots: { el: HTMLElement; left: number }[]) {
    for (const { el, left: oldLeft } of snapshots) {
      const newLeft = el.getBoundingClientRect().left;
      const diff = oldLeft - newLeft;
      if (Math.abs(diff) < 0.5) continue;
      el.style.transition = "none";
      el.style.transform = `translateX(${diff}px)`;
      el.getBoundingClientRect(); // force reflow so transition fires
      el.style.transition = "transform 0.15s ease";
      el.style.transform = "";
    }
  }

  // Reinsert dragEl before `anchor` and update naturalLeft.
  function reinsertBefore(anchor: HTMLElement | null) {
    if (!dragEl) return;
    const addBtn = document.getElementById("tabAddButton");

    // Snapshot non-drag tab positions before the DOM change.
    const snapshots = tabs
      .filter(t => t.tabEl !== dragEl)
      .map(t => ({ el: t.tabEl, left: t.tabEl.getBoundingClientRect().left }));

    tabBar.insertBefore(dragEl, anchor ?? addBtn);

    // Measure natural position via offsetLeft (unaffected by CSS transforms).
    dragEl.style.transform = "";
    naturalLeft = dragEl.offsetLeft - tabBar.offsetLeft;

    flipSiblings(snapshots);
  }

  tabBar.addEventListener("pointerdown", (e: PointerEvent) => {
    const tab = (e.target as HTMLElement).closest(".splitTab") as HTMLElement | null;
    if (!tab) return;
    if ((e.target as HTMLElement).closest(".tabClose")) return;

    // Activate tab immediately on press.
    const entry = tabs.find(t => t.tabEl === tab);
    if (entry) activateTab(entry.docId, pageDocId);

    dragEl = tab;
    startClientX = e.clientX;
    barLeft = tabBar.getBoundingClientRect().left;
    const tabRect = tab.getBoundingClientRect();
    dragOffsetX = e.clientX - tabRect.left;
    naturalLeft = tab.offsetLeft - tabBar.offsetLeft;
    isDragging = false;

    tab.setPointerCapture(e.pointerId);
    e.preventDefault();
  });

  tabBar.addEventListener("pointermove", (e: PointerEvent) => {
    if (!dragEl) return;
    if (!isDragging) {
      if (Math.abs(e.clientX - startClientX) < 4) return;
      isDragging = true;
      dragEl.classList.add("dragging");
    }

    const tabWidth = dragEl.offsetWidth;
    const barWidth = tabBar.offsetWidth;
    const desiredLeft = Math.max(
      0,
      Math.min(barWidth - tabWidth, e.clientX - barLeft - dragOffsetX)
    );

    dragEl.style.transition = "none";
    dragEl.style.transform = `translateX(${desiredLeft - naturalLeft}px)`;

    const dragIdx = tabs.findIndex(t => t.tabEl === dragEl);

    // Swap with left neighbour?
    if (dragIdx > 0) {
      const leftEl = tabs[dragIdx - 1].tabEl;
      const leftThreshold = leftEl.offsetLeft - tabBar.offsetLeft + leftEl.offsetWidth * 0.50;
      if (desiredLeft < leftThreshold) {
        const [moved] = tabs.splice(dragIdx, 1);
        tabs.splice(dragIdx - 1, 0, moved);
        reinsertBefore(leftEl);
        dragEl.style.transition = "none";
        dragEl.style.transform = `translateX(${desiredLeft - naturalLeft}px)`;
        return;
      }
    }

    // Swap with right neighbour?
    const curIdx = tabs.findIndex(t => t.tabEl === dragEl);
    if (curIdx < tabs.length - 1) {
      const rightEl = tabs[curIdx + 1].tabEl;
      const rightThreshold = rightEl.offsetLeft - tabBar.offsetLeft + rightEl.offsetWidth * 0.50;
      if (desiredLeft + tabWidth > rightThreshold) {
        const [moved] = tabs.splice(curIdx, 1);
        tabs.splice(curIdx + 1, 0, moved);
        const nextAnchor = (tabs[curIdx + 2]?.tabEl ?? document.getElementById("tabAddButton")) as HTMLElement;
        reinsertBefore(nextAnchor);
        dragEl.style.transition = "none";
        dragEl.style.transform = `translateX(${desiredLeft - naturalLeft}px)`;
      }
    }
  });

  const endDrag = () => {
    if (!dragEl) return;
    const el = dragEl;
    dragEl = null;
    isDragging = false;
    el.classList.remove("dragging");
    el.style.transition = "transform 0.15s ease";
    el.style.transform = "";
    el.addEventListener("transitionend", () => { el.style.transition = ""; }, { once: true });
  };

  const cancelDrag = () => {
    if (!dragEl) return;
    dragEl.classList.remove("dragging");
    dragEl.style.transition = "";
    dragEl.style.transform = "";
    // Also clear any in-flight sibling transitions
    for (const t of tabs) {
      t.tabEl.style.transition = "";
      t.tabEl.style.transform = "";
    }
    dragEl = null;
    isDragging = false;
  };

  tabBar.addEventListener("pointerup", endDrag);
  tabBar.addEventListener("pointercancel", cancelDrag);
}

// ── Tab management ───────────────────────────────────────────────────

function activateTab(docId: T.DocId, pageDocId: string) {
  // Save scroll of currently active tab
  if (activeDocId && activeDocId !== docId) {
    const prev = tabs.find(t => t.docId === activeDocId);
    if (prev) {
      const key = `split-scroll:${pageDocId}:${activeDocId}`;
      localStorage.setItem(key, prev.editorContainerEl.scrollTop.toString());
    }
  }

  activeDocId = docId;

  for (const t of tabs) {
    t.tabEl.classList.toggle("active", t.docId === docId);
    t.editorContainerEl.classList.toggle("active", t.docId === docId);
  }

  // Restore scroll for newly active tab
  const entry = tabs.find(t => t.docId === docId);
  if (entry) {
    const key = `split-scroll:${pageDocId}:${docId}`;
    const saved = parseInt(localStorage.getItem(key) ?? '', 10);
    requestAnimationFrame(() => {
      entry.editorContainerEl.scrollTop = isNaN(saved) ? 0 : saved;
    });
  }
}

function updateAddButtonVisibility() {
  const tabAddButton = document.getElementById("tabAddButton");
  if (!tabAddButton) return;
  const allOpen = allDocs.every(d => tabs.some(t => t.docId === d.docId));
  tabAddButton.classList.toggle("hidden", allOpen);
}

function openTab(docId: T.DocId, ws: WS.MyWebsocket, pageDocId: string) {
  // Don't open duplicate tabs
  if (tabs.some(t => t.docId === docId)) {
    activateTab(docId, pageDocId);
    if (!pickerMultiSelect) closeMemberPicker();
    return;
  }

  const label = labelMap.get(docId) ?? "?";

  // Create editor container
  const splitEditorArea = document.getElementById("splitEditorArea");
  if (!splitEditorArea) return;
  const editorContainerEl = document.createElement("div");
  editorContainerEl.className = "splitTabEditor editorHolder";
  splitEditorArea.appendChild(editorContainerEl);

  // Create tab element
  const tabEl = document.createElement("button");
  tabEl.className = "splitTab";
  tabEl.innerHTML = `
    <span class="tabLabel">${escapeHtml(label)}</span>
    <button class="tabClose" title="Close tab">
      <img src="/static/img/x.svg" alt="Close">
    </button>
  `;

  // Click on tab → activate
  tabEl.addEventListener("click", (e) => {
    if ((e.target as HTMLElement).closest(".tabClose")) return;
    activateTab(docId, pageDocId);
  });

  // Click on close button
  tabEl.querySelector(".tabClose")?.addEventListener("click", (e) => {
    e.stopPropagation();
    closeTab(docId, ws, pageDocId);
  });

  // Insert tab before #tabAddButton
  const tabAddButton = document.getElementById("tabAddButton");
  const tabBar = document.getElementById("tabBar");
  if (tabBar && tabAddButton) {
    tabBar.insertBefore(tabEl, tabAddButton);
  }

  const entry: TabEntry = {
    docId,
    label,
    tabEl,
    editorContainerEl,
    editor: null,
    isLoading: true,
  };
  tabs.push(entry);

  // Attach scroll listener so position is saved continuously
  makeScrollSaver(editorContainerEl, pageDocId, docId);

  // Open the panel if not already
  const studyPage = document.getElementById("studyPage");
  studyPage?.classList.add("split");

  activateTab(docId, pageDocId);

  // Subscribe via WS
  listenQueue.push(docId);
  ws.send({ tag: "ListenToDoc", contents: docId });

  updateAddButtonVisibility();
  if (pickerMultiSelect) {
    renderMemberPicker(pickerWs!, pickerPageDocId, false);
  } else {
    closeMemberPicker();
  }
}

function closeTab(docId: T.DocId, ws: WS.MyWebsocket, pageDocId: string) {
  const idx = tabs.findIndex(t => t.docId === docId);
  if (idx === -1) return;
  const entry = tabs[idx];

  ws.send({ tag: "StopListenToDoc", contents: docId });

  // Remove from listenQueue if still pending
  listenQueue = listenQueue.filter(id => id !== docId);

  entry.editor?.removeEditor();
  entry.tabEl.remove();
  entry.editorContainerEl.remove();

  tabs.splice(idx, 1);

  if (tabs.length === 0) {
    closeSidePanel(ws);
    return;
  }

  if (activeDocId === docId) {
    // Activate adjacent tab (prefer right, fallback left)
    const nextTab = tabs[idx] ?? tabs[idx - 1];
    if (nextTab) activateTab(nextTab.docId, pageDocId);
  }

  updateAddButtonVisibility();
}

function closeSidePanel(ws: WS.MyWebsocket) {
  for (const entry of tabs) {
    ws.send({ tag: "StopListenToDoc", contents: entry.docId });
    entry.editor?.removeEditor();
  }
  tabs = [];
  activeDocId = undefined;
  listenQueue = [];

  const splitEditorArea = document.getElementById("splitEditorArea");
  if (splitEditorArea) splitEditorArea.innerHTML = "";

  const tabBar = document.getElementById("tabBar");
  const tabAddButton = document.getElementById("tabAddButton");
  if (tabBar && tabAddButton) {
    // Remove all children except the add button
    Array.from(tabBar.children).forEach(child => {
      if (child !== tabAddButton) child.remove();
    });
  }

  const studyPage = document.getElementById("studyPage");
  studyPage?.classList.remove("split");

  updateAddButtonVisibility();
}

// ── Member picker ────────────────────────────────────────────────────

function nameShort(fullName: string): string {
  const parts = fullName.trim().split(/\s+/);
  const first = parts[0]?.[0] ?? "?";
  const last = parts.length > 1 ? (parts[parts.length - 1]?.[0] ?? "") : "";
  return (first + last).toUpperCase();
}

function escapeHtml(str: string): string {
  return str
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

let pickerDismissHandler: ((e: MouseEvent) => void) | null = null;
let pickerMultiSelect = false;
let pickerWs: WS.MyWebsocket | null = null;
let pickerPageDocId: string = "";

function renderMemberPicker(ws: WS.MyWebsocket, pageDocId: string, filterOpen: boolean = false) {
  const popover = document.getElementById("memberPickerPopover");
  if (!popover) return;
  popover.innerHTML = "";

  // Build rows for each owner in the group
  const groupStudyData = loadGroupStudyData(pageDocId);

  // Show docs for owners (filtered to non-self, which allDocs already is)
  for (const doc of allDocs) {
    const isOpen = tabs.some(t => t.docId === doc.docId);

    // When opened from the plus button, skip already-open docs
    if (filterOpen && isOpen) continue;

    const label = labelMap.get(doc.docId) ?? doc.editors[0]?.name ?? "?";

    const row = document.createElement("div");
    row.className = "memberPickerRow" + (isOpen ? " open" : "");

    const avatar = document.createElement("span");
    avatar.className = "memberPickerAvatar";
    avatar.textContent = nameShort(doc.editors[0]?.name ?? label);

    const name = document.createElement("span");
    name.className = "memberPickerName";
    name.textContent = label;

    row.appendChild(avatar);
    row.appendChild(name);

    if (isOpen) {
      const check = document.createElement("span");
      check.className = "memberPickerCheck";
      check.textContent = "Viewing";
      row.appendChild(check);
      if (!filterOpen) {
        row.addEventListener("click", () => {
          closeTab(doc.docId, ws, pageDocId);
          renderMemberPicker(ws, pageDocId, false);
        });
        row.style.cursor = "pointer";
      }
    } else {
      row.addEventListener("click", () => {
        openTab(doc.docId, ws, pageDocId);
      });
    }

    popover.appendChild(row);
  }

  // Owners without a doc (skip the current user — their doc is excluded from allDocs)
  const currentUserIds = new Set(
    groupStudyData.docs
      .filter(d => d.docId === (pageDocId as T.DocId))
      .flatMap(d => d.editors.map(e => e.userId))
  );
  for (const owner of groupStudyData.owners) {
    if (currentUserIds.has(owner.userId)) continue;
    const hasDocs = allDocs.some(d => d.editors.some(e => e.userId === owner.userId));
    if (!hasDocs) {
      if (filterOpen) continue; // no doc = can't open, skip in filter mode
      const row = document.createElement("div");
      row.className = "memberPickerRow noDoc";

      const avatar = document.createElement("span");
      avatar.className = "memberPickerAvatar";
      avatar.textContent = nameShort(owner.name);

      const noDocName = document.createElement("span");
      noDocName.className = "memberPickerName";
      noDocName.textContent = `${owner.name} — No document yet`;

      row.appendChild(avatar);
      row.appendChild(noDocName);
      popover.appendChild(row);
    }
  }
}

function openMemberPicker(anchorEl: HTMLElement, ws: WS.MyWebsocket, pageDocId: string, filterOpen: boolean = false, multiSelect: boolean = false) {
  const popover = document.getElementById("memberPickerPopover");
  if (!popover) return;

  pickerMultiSelect = multiSelect;
  pickerWs = ws;
  pickerPageDocId = pageDocId;

  renderMemberPicker(ws, pageDocId, filterOpen);
  popover.classList.remove("hidden");

  const rect = anchorEl.getBoundingClientRect();
  popover.style.top = `${rect.bottom + 4}px`;
  popover.style.left = `${rect.left}px`;

  // Dismiss on outside click
  if (pickerDismissHandler) {
    document.removeEventListener("click", pickerDismissHandler);
  }
  pickerDismissHandler = (e: MouseEvent) => {
    const target = e.target as Node;
    // Ignore clicks on elements that were removed from DOM during re-render
    if (!document.contains(target)) return;
    if (!popover.contains(target) && target !== anchorEl) {
      closeMemberPicker();
    }
  };
  // Use setTimeout to avoid the current click triggering dismiss immediately
  setTimeout(() => {
    document.addEventListener("click", pickerDismissHandler!);
  }, 0);
}

function closeMemberPicker() {
  const popover = document.getElementById("memberPickerPopover");
  popover?.classList.add("hidden");
  if (pickerDismissHandler) {
    document.removeEventListener("click", pickerDismissHandler);
    pickerDismissHandler = null;
  }
  pickerMultiSelect = false;
}

// ── Data loading ─────────────────────────────────────────────────────

function loadGroupStudyData(pageDocId: string): T.GroupStudyRaw {
  const el = document.getElementById("groupStudyData");
  if (!el) return { name: "", studyId: "" as T.GroupStudyId, studyTemplateId: "" as T.StudyTemplateId, docs: [], owners: [] };
  const raw = JSON.parse(el.textContent ?? "{}") as T.GroupStudyRaw;
  return raw;
}

// ── Entry point ──────────────────────────────────────────────────────

export function init(ws: WS.MyWebsocket) {
  const splitscreenButton = document.getElementById("splitscreenButton");
  if (!splitscreenButton) return;

  const pageDocId = window.pageDocId ?? window.location.pathname.split('/').pop() ?? '';

  // Load group study data and compute labels (exclude own doc)
  const groupStudy = loadGroupStudyData(pageDocId);
  allDocs = groupStudy.docs.filter(d => d.docId !== (pageDocId as T.DocId));
  labelMap = computeLabels(allDocs);

  const splitsideClose = document.getElementById("splitsideClose");
  const tabAddButton = document.getElementById("tabAddButton");

  splitscreenButton.addEventListener("click", () => {
    openMemberPicker(splitscreenButton, ws, pageDocId, false, false);
  });

  tabAddButton?.addEventListener("click", () => {
    openMemberPicker(tabAddButton, ws, pageDocId, true);
  });

  splitsideClose?.addEventListener("click", () => {
    closeSidePanel(ws);
  });

  initTabDrag(pageDocId);

  // WS: DocListenStart — FIFO queue routes response to correct tab
  ws.addEventListener("DocListenStart", (ev: WS.DocListenStartEvent) => {
    const docId = listenQueue.shift();
    if (!docId) return;
    const entry = tabs.find(t => t.docId === docId);
    if (!entry) return; // Tab was closed before response arrived

    entry.isLoading = false;
    entry.editor = new Editor.P215Editor({
      initDoc: ev.document,
      editable: false,
      remoteThings: null,
    });
    entry.editor.addEditor(entry.editorContainerEl);

    if (docId === activeDocId) {
      const key = `split-scroll:${pageDocId}:${docId}`;
      const saved = parseInt(localStorage.getItem(key) ?? '', 10);
      requestAnimationFrame(() => {
        entry.editorContainerEl.scrollTop = isNaN(saved) ? 0 : saved;
      });
    }
  });

  // WS: DocUpdated — route to correct tab editor
  ws.addEventListener("DocUpdated", (ev: WS.DocUpdatedEvent) => {
    const { docId, update } = ev.contents;
    const entry = tabs.find(t => t.docId === docId);
    entry?.editor?.dispatchSteps(update);
  });

  // WS: Reconnect — re-subscribe all open tabs
  ws.addEventListener("open", () => {
    if (tabs.length === 0) return;
    // Re-populate queue and re-send ListenToDoc for each tab
    for (const entry of tabs) {
      listenQueue.push(entry.docId);
      ws.send({ tag: "ListenToDoc", contents: entry.docId });
    }
  });
}
