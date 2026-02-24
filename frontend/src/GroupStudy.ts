import * as WS from "./WebsocketTypes"
import * as T from "./Types"
import * as Editor from "./Editor/Editor"
import * as Util from "./Util"

export function init(ws : WS.MyWebsocket) {
  const splitscreenButton = document.getElementById("splitscreenButton");
  if(splitscreenButton) {
    const groupStudySelector =
      document.getElementById("groupStudySelector") as HTMLSelectElement ;

    let editor : Editor.P215Editor = null;
    let currentDocId : T.DocId;

    groupStudySelector.addEventListener("input", () => {
      loadEditor();
    });

      splitscreenButton.addEventListener("click", () => {
        loadEditor();
      });

    function loadEditor() {
      const docId = groupStudySelector.value as T.DocId;
      if (docId == currentDocId) return;
      if(currentDocId) {
        ws.send({ tag: "StopListenToDoc", contents: docId });
      }
      currentDocId = docId;
      ws.send({ tag: "ListenToDoc", contents: docId });
    }

    ws.addEventListener("DocListenStart", (ev : WS.DocListenStartEvent) => {
      if(editor) {
        editor.removeEditor();
      }
      editor = new Editor.P215Editor({
        initDoc: ev.document,
        editable: false,
        initialVersion: ev.snapVersion,
        sessionClientId: "listener-" + Util.getRandomStr(),
        remoteThings: null,
      });
      const elem = document.getElementById("sideBySideEditor");
      editor.addEditor(elem);
      if (ev.pendingSteps.length > 0) {
        editor.dispatchSteps({ steps: ev.pendingSteps, clientIds: ev.pendingClientIds });
      }
    });

    ws.addEventListener("DocUpdated", (ev : WS.DocUpdatedEvent) => {
      if (ev.update.docId === currentDocId && editor) {
        editor.dispatchSteps(ev.update);
      }
    });
  }
}
