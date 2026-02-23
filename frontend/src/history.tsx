import * as Editor from "./Editor/Editor";
import { textSchema } from "./Editor/textSchema";
import { Node } from "prosemirror-model";
import { Step, Transform } from "prosemirror-transform";

declare global {
  interface Window {
    docId: string;
    docName: string;
    isLocal: number;
    base: string;
  }
}

const docId = window.docId;

type HistoryGroup = {
  startVersion: number;
  endVersion: number;
  startedAt: string;
  endedAt: string;
  stepCount: number;
};

type SubHistoryGroup = HistoryGroup;

type DocAtVersion = {
  snapshotDoc: any;
  steps: any[];
  fromVersion: number;
  toVersion: number;
};

let previewEditor: Editor.P215Editor | null = null;

function formatGroupLabel(iso: string): string {
  const d = new Date(iso);
  return (
    d.toLocaleDateString(undefined, { month: "long", day: "numeric" }) +
    ", " +
    d.toLocaleTimeString(undefined, { hour: "numeric", minute: "2-digit" })
  );
}

function formatTime(iso: string): string {
  return new Date(iso).toLocaleTimeString(undefined, {
    hour: "numeric",
    minute: "2-digit",
  });
}

function getPeriodLabel(startedAt: string): string {
  const now = new Date();
  const todayStart = new Date(now.getFullYear(), now.getMonth(), now.getDate());
  const d = new Date(startedAt);
  const dStart = new Date(d.getFullYear(), d.getMonth(), d.getDate());
  const daysDiff = Math.floor((todayStart.getTime() - dStart.getTime()) / (1000 * 60 * 60 * 24));

  if (daysDiff === 0) return "Today";
  if (daysDiff >= 1 && daysDiff <= 6) {
    return d.toLocaleDateString(undefined, { weekday: "long" });
  }
  if (d.getFullYear() === now.getFullYear() && d.getMonth() === now.getMonth()) {
    return "This Month";
  }
  if (d.getFullYear() === now.getFullYear()) {
    return d.toLocaleDateString(undefined, { month: "long" });
  }
  return d.toLocaleDateString(undefined, { month: "long", year: "numeric" });
}

function groupByPeriod(groups: HistoryGroup[]): Map<string, HistoryGroup[]> {
  const map = new Map<string, HistoryGroup[]>();
  for (const g of groups) {
    const label = getPeriodLabel(g.startedAt);
    if (!map.has(label)) map.set(label, []);
    map.get(label)!.push(g);
  }

  const now = new Date();

  // Day labels: yesterday first, counting back 6 days
  const dynamicDayLabels: string[] = [];
  for (let i = 1; i <= 6; i++) {
    const d = new Date(now.getFullYear(), now.getMonth(), now.getDate() - i);
    dynamicDayLabels.push(d.toLocaleDateString(undefined, { weekday: "long" }));
  }

  // Month labels: last month going back to the oldest item
  const oldest = groups.reduce((min, g) => {
    const d = new Date(g.startedAt);
    return d < min ? d : min;
  }, new Date());

  const monthLabels: string[] = [];
  let year = now.getFullYear();
  let month = now.getMonth() - 1;
  if (month < 0) { month = 11; year--; }

  while (year > oldest.getFullYear() || (year === oldest.getFullYear() && month >= oldest.getMonth())) {
    const d = new Date(year, month, 1);
    monthLabels.push(
      year === now.getFullYear()
        ? d.toLocaleDateString(undefined, { month: "long" })
        : d.toLocaleDateString(undefined, { month: "long", year: "numeric" })
    );
    month--;
    if (month < 0) { month = 11; year--; }
  }

  const orderedLabels = ["Today", ...dynamicDayLabels, "This Month", ...monthLabels];
  const sorted = new Map<string, HistoryGroup[]>();
  for (const label of orderedLabels) {
    if (map.has(label)) {
      const items = map.get(label)!.slice().sort((a, b) =>
        new Date(b.startedAt).getTime() - new Date(a.startedAt).getTime()
      );
      sorted.set(label, items);
    }
  }
  return sorted;
}

async function loadPreview(targetVersion: number, versionLabel?: string) {
  const res = await fetch(`/document/${docId}/at-version/${targetVersion}`);
  if (!res.ok) return;
  const data: DocAtVersion = await res.json();

  const isEmpty = !data.snapshotDoc || Object.keys(data.snapshotDoc).length === 0;
  const snapshotNode = isEmpty
    ? (textSchema.topNodeType.createAndFill() ?? textSchema.nodeFromJSON({ type: "doc", content: [] }))
    : Node.fromJSON(textSchema, data.snapshotDoc);
  if (!snapshotNode) return;
  let tr = new Transform(snapshotNode);
  for (const s of data.steps) {
    try {
      tr.step(Step.fromJSON(textSchema, s));
    } catch (e) {
      console.warn("Could not apply step", e);
    }
  }
  const previewDoc = tr.doc;

  const holder = document.getElementById("editorHolder");

  if (previewEditor) {
    previewEditor.removeEditor();
    previewEditor = null;
  }

  previewEditor = new Editor.P215Editor({
    initDoc: previewDoc.toJSON(),
    editable: false,
    remoteThings: null,
  });
  previewEditor.addEditor(holder);

  document.getElementById("historyPreviewPlaceholder").style.display = "none";
  document.getElementById("previewToolbar").style.display = "flex";
  document.getElementById("editorHolder").style.display = "block";
  document.getElementById("historyLayout").classList.add("preview-open");

  document.getElementById("previewToolbarDate").textContent = versionLabel ?? "";
  updateToolbarPill();

  const restoreButton = document.getElementById("restoreButton") as HTMLButtonElement;
  restoreButton.onclick = () => {
    sessionStorage.setItem(
      docId + ".restore",
      JSON.stringify({ docJson: previewDoc.toJSON(), versionNum: targetVersion })
    );
    window.location.href = `/study/${docId}?restore=true`;
  };
}

function selectItem(el: Element) {
  document.querySelectorAll(".historyGroupItem.selected, .historyStepItem.selected").forEach((e) =>
    e.classList.remove("selected")
  );
  el.classList.add("selected");
}

async function loadSubGroups(
  group: HistoryGroup,
  panel: HTMLElement,
): Promise<void> {
  panel.textContent = "Loading…";
  const res = await fetch(
    `/document/${docId}/history/sub/${group.startVersion}/${group.endVersion}`
  );
  if (!res.ok) {
    panel.textContent = "Failed to load.";
    return;
  }
  const subGroups: SubHistoryGroup[] = await res.json();
  panel.textContent = "";

  if (subGroups.length === 0) {
    panel.textContent = "No sub-items.";
    return;
  }

  for (const sub of subGroups) {
    const btn = document.createElement("button");
    btn.className = "historyStepItem";
    btn.innerHTML =
      `<span class="historyStepTime">${formatTime(sub.startedAt)}</span>` +
      `<span class="historyStepCount">${sub.stepCount} edit${sub.stepCount !== 1 ? "s" : ""}</span>`;
    btn.addEventListener("click", (e) => {
      e.stopPropagation();
      selectItem(btn);
      loadPreview(sub.endVersion, formatTime(sub.startedAt));
    });
    panel.appendChild(btn);
  }
}

function updateToolbarPill() {
  const toolbar = document.getElementById("previewToolbar");
  const pill = document.getElementById("previewToolbarDate") as HTMLElement;
  if (toolbar.style.display === "none") return;
  pill.style.display = "";
  if (toolbar.scrollWidth > toolbar.clientWidth) {
    pill.style.display = "none";
  }
}

function initToolbarPillObserver() {
  const toolbar = document.getElementById("previewToolbar");
  new ResizeObserver(updateToolbarPill).observe(toolbar);
}

function initBackButton() {
  document.getElementById("backToVersions").addEventListener("click", () => {
    document.getElementById("historyLayout").classList.remove("preview-open");
    document.querySelectorAll(".historyGroupItem.selected, .historyStepItem.selected").forEach((e) =>
      e.classList.remove("selected")
    );
  });
}

async function init() {
  const res = await fetch(`/document/${docId}/history`);
  if (!res.ok) {
    document.getElementById("historyGroups").textContent = "Failed to load history.";
    return;
  }
  const groups: HistoryGroup[] = await res.json();

  const container = document.getElementById("historyGroups");
  container.textContent = "";

  if (groups.length === 0) {
    container.textContent = "No history yet. Start editing to record versions.";
    return;
  }

  const byPeriod = groupByPeriod(groups);

  for (const [periodLabel, periodGroups] of byPeriod) {
    const section = document.createElement("div");
    section.className = "historyDateSection";

    const heading = document.createElement("h3");
    heading.className = "historyDateHeading";
    heading.textContent = periodLabel;
    section.appendChild(heading);

    for (const group of periodGroups) {
      const accordion = document.createElement("div");
      accordion.className = "historyAccordion";

      // The row: clickable area (preview) + chevron button (expand)
      const row = document.createElement("div");
      row.className = "historyGroupItem";

      const previewBtn = document.createElement("button");
      previewBtn.className = "historyGroupPreviewBtn";
      previewBtn.innerHTML =
        `<span class="historyTime">${formatGroupLabel(group.endedAt)}</span>` +
        `<span class="historySteps">${group.stepCount} edit${group.stepCount !== 1 ? "s" : ""}</span>`;

      const expandBtn = document.createElement("button");
      expandBtn.className = "historyExpandBtn";
      expandBtn.setAttribute("aria-label", "Expand version details");
      expandBtn.setAttribute("aria-expanded", "false");
      expandBtn.innerHTML = `<img src="${window.base}/static/img/chevron-down.svg" alt="" aria-hidden="true">`;

      const stepsPanel = document.createElement("div");
      stepsPanel.className = "historyStepsPanel";
      stepsPanel.style.display = "none";
      const panelId = `steps-panel-${group.startVersion}`;
      stepsPanel.id = panelId;
      expandBtn.setAttribute("aria-controls", panelId);

      let subLoaded = false;

      previewBtn.addEventListener("click", () => {
        selectItem(row);
        loadPreview(group.endVersion, formatGroupLabel(group.endedAt));
      });

      expandBtn.addEventListener("click", async (e) => {
        e.stopPropagation();
        const isOpen = stepsPanel.style.display !== "none";
        if (isOpen) {
          stepsPanel.style.display = "none";
          expandBtn.classList.remove("expanded");
          expandBtn.setAttribute("aria-expanded", "false");
        } else {
          stepsPanel.style.display = "block";
          expandBtn.classList.add("expanded");
          expandBtn.setAttribute("aria-expanded", "true");
          if (!subLoaded) {
            subLoaded = true;
            await loadSubGroups(group, stepsPanel);
          }
        }
      });

      row.appendChild(previewBtn);
      row.appendChild(expandBtn);
      accordion.appendChild(row);
      accordion.appendChild(stepsPanel);
      section.appendChild(accordion);
    }

    container.appendChild(section);
  }
}

init();
initBackButton();
initToolbarPillObserver();
