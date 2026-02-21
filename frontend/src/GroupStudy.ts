import * as WS from "./WebsocketTypes"
import * as T from "./Types"
import * as Editor from "./Editor/Editor"

function makeScrollMemory(container: HTMLElement, pageDocId: string) {
  const key = (docId: T.DocId) => `split-scroll:${pageDocId}:${docId}`;

  function save(docId: T.DocId) {
    localStorage.setItem(key(docId), container.scrollTop.toString());
  }

  function restore(docId: T.DocId) {
    const saved = localStorage.getItem(key(docId));
    requestAnimationFrame(() => {
      container.scrollTop = saved !== null ? parseInt(saved, 10) : 0;
    });
  }

  function watch(getCurrentDocId: () => T.DocId | undefined) {
    let timer: ReturnType<typeof setTimeout> | null = null;
    container.addEventListener("scroll", () => {
      if (timer) clearTimeout(timer);
      timer = setTimeout(() => {
        const docId = getCurrentDocId();
        if (docId) save(docId);
      }, 200);
    });
  }

  return { save, restore, watch };
}

export function init(ws : WS.MyWebsocket) {
  const splitscreenButton = document.getElementById("splitscreenButton");
  if (!splitscreenButton) return;

  const groupStudySelector =
    document.getElementById("groupStudySelector") as HTMLSelectElement;
  const splitside = document.getElementById("splitside");
  const pageDocId = window.location.pathname.split('/').pop() ?? '';

  let editor : Editor.P215Editor = null;
  let currentDocId : T.DocId | undefined;

  const scroll = splitside ? makeScrollMemory(splitside, pageDocId) : null;
  scroll?.watch(() => currentDocId);

  groupStudySelector.addEventListener("input", loadEditor);
  splitscreenButton.addEventListener("click", loadEditor);

  function loadEditor() {
    const docId = groupStudySelector.value as T.DocId;
    if (docId === currentDocId) return;
    if (currentDocId) {
      scroll?.save(currentDocId);
      ws.send({ tag: "StopListenToDoc", contents: currentDocId });
    }
    currentDocId = docId;
    ws.send({ tag: "ListenToDoc", contents: docId });
  }

  ws.addEventListener("DocListenStart", (ev : WS.DocListenStartEvent) => {
    console.log(ev.document);
    editor?.removeEditor();
    editor = new Editor.P215Editor({
      initDoc: ev.document,
      editable: false,
      remoteThings: null,
    });
    editor.addEditor(document.getElementById("sideBySideEditor"));
    scroll?.restore(currentDocId);
  });

  ws.addEventListener("DocUpdated", (ev : WS.DocUpdatedEvent) => {
    editor?.dispatchSteps(ev.update);
  });
}
