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

function formatDate(iso: string): string {
  const d = new Date(iso);
  return d.toLocaleDateString(undefined, {
    month: "short",
    day: "numeric",
    year: "numeric",
  }) + " " + d.toLocaleTimeString(undefined, { hour: "2-digit", minute: "2-digit" });
}

function formatTime(iso: string): string {
  return new Date(iso).toLocaleTimeString(undefined, {
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
  });
}

function groupByDate(groups: HistoryGroup[]): Map<string, HistoryGroup[]> {
  const map = new Map<string, HistoryGroup[]>();
  for (const g of groups) {
    const date = new Date(g.startedAt).toLocaleDateString(undefined, {
      weekday: "long", month: "long", day: "numeric", year: "numeric",
    });
    if (!map.has(date)) map.set(date, []);
    map.get(date).push(g);
  }
  return map;
}

async function loadPreview(targetVersion: number) {
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
  document.getElementById("historyPreviewActions").style.display = "flex";
  document.getElementById("editorHolder").style.display = "block";

  const restoreButton = document.getElementById("restoreButton");
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
      loadPreview(sub.endVersion);
    });
    panel.appendChild(btn);
  }
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

  const byDate = groupByDate(groups);

  for (const [dateLabel, dayGroups] of byDate) {
    const section = document.createElement("div");
    section.className = "historyDateSection";

    const heading = document.createElement("h3");
    heading.className = "historyDateHeading";
    heading.textContent = dateLabel;
    section.appendChild(heading);

    for (const group of dayGroups) {
      const accordion = document.createElement("div");
      accordion.className = "historyAccordion";

      const header = document.createElement("button");
      header.className = "historyGroupItem";
      header.innerHTML =
        `<span class="historyTime">${formatDate(group.startedAt)}</span>` +
        `<span class="historySteps">${group.stepCount} edit${group.stepCount !== 1 ? "s" : ""}</span>`;

      const stepsPanel = document.createElement("div");
      stepsPanel.className = "historyStepsPanel";
      stepsPanel.style.display = "none";

      let subLoaded = false;

      header.setAttribute("aria-expanded", "false");
      header.setAttribute("aria-controls", stepsPanel.id || (() => {
        const id = `steps-panel-${group.startVersion}`;
        stepsPanel.id = id;
        return id;
      })());

      header.addEventListener("click", async () => {
        const isOpen = stepsPanel.style.display !== "none";
        if (isOpen) {
          stepsPanel.style.display = "none";
          header.classList.remove("expanded");
          header.setAttribute("aria-expanded", "false");
        } else {
          stepsPanel.style.display = "block";
          header.classList.add("expanded");
          header.setAttribute("aria-expanded", "true");
          selectItem(header);
          loadPreview(group.endVersion);
          if (!subLoaded) {
            subLoaded = true;
            await loadSubGroups(group, stepsPanel);
          }
        }
      });

      accordion.appendChild(header);
      accordion.appendChild(stepsPanel);
      section.appendChild(accordion);
    }

    container.appendChild(section);
  }
}

init();
