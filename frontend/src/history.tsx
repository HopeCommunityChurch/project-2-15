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

async function loadPreview(group: HistoryGroup) {
  const res = await fetch(`/api/document/${docId}/at-version/${group.endVersion}`);
  if (!res.ok) return;
  const data: DocAtVersion = await res.json();

  const snapshotNode = Node.fromJSON(textSchema, data.snapshotDoc);
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
  document.getElementById("historyPreviewActions").style.display = "block";

  const restoreButton = document.getElementById("restoreButton");
  restoreButton.onclick = () => {
    sessionStorage.setItem(
      docId + ".restore",
      JSON.stringify({ docJson: previewDoc.toJSON(), versionNum: group.endVersion })
    );
    window.location.href = `/study/${docId}?restore=true`;
  };
}

async function init() {
  const res = await fetch(`/api/document/${docId}/history`);
  if (!res.ok) {
    document.getElementById("historyGroups").textContent = "Failed to load history.";
    return;
  }
  const groups: HistoryGroup[] = await res.json();

  const container = document.getElementById("historyGroups");

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
      const item = document.createElement("button");
      item.className = "historyGroupItem";
      item.innerHTML =
        `<span class="historyTime">${formatDate(group.startedAt)}</span>` +
        `<span class="historySteps">${group.stepCount} edit${group.stepCount !== 1 ? "s" : ""}</span>`;
      item.addEventListener("click", () => {
        document.querySelectorAll(".historyGroupItem.selected").forEach((el) =>
          el.classList.remove("selected")
        );
        item.classList.add("selected");
        loadPreview(group);
      });
      section.appendChild(item);
    }

    container.appendChild(section);
  }
}

init();
