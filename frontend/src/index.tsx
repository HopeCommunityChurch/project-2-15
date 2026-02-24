
import * as EActions from "./Editor/editorUtils"
import * as Editor from "./Editor/Editor"
import * as WS from "./WebsocketTypes"
import * as T from "./Types"
import {} from "./PreviewScripture"
import {} from "./StudyBlockEditor"
import {} from "./SidebarSections"
import * as GS from "./GroupStudy"
import * as Util from "./Util"


const pathparts = window.location.pathname.split("/");
const docId = pathparts[pathparts.length - 1] as T.DocId;

const ws = new WS.MyWebsocket();
ws.connect();

const computerId = (function () : string {
  let computerId = window.localStorage.getItem("computerId");
  if (computerId === null) {
    computerId = Util.getRandomStr();
    window.localStorage.setItem("computerId", computerId);
  }
  return computerId;
})();

const sessionClientId = computerId + "_" + Util.getRandomStr();


ws.addEventListener("open", () => {
  ws.sendStrRaw(computerId);
  ws.openDoc(docId);
});

const localSaveTimeKey = docId +  ".time"
const lastRemoveSaveTimeKey = docId +  ".remoteSaveTime"
const localDoc = docId + ".doc";


function checkRestoreDoc(doc: T.DocRaw): any | null {
  const restoreFlag = new URLSearchParams(location.search).get("restore");
  if (!restoreFlag) return null;
  const saved = sessionStorage.getItem(docId + ".restore");
  if (!saved) return null;
  sessionStorage.removeItem(docId + ".restore");
  try {
    // versionNum is also stored here for future use (e.g. display "restored from v42").
    // Do not remove it from history.tsx without updating this site too.
    const { docJson } = JSON.parse(saved);
    return docJson;
  } catch (e) {
    console.error("failed to parse restore data", e);
    return null;
  }
}

let wasInitialized = false;

function initialize (e : WS.DocOpenedEvent) {
  const saver = mkSaveObject(ws);

  const restoredDoc = checkRestoreDoc(e.doc);

  // Always use the server-provided snapshot document. For restores, use the
  // restored content at the current server version.
  const initDoc = restoredDoc !== null ? restoredDoc : e.doc.document;
  const initVersion = restoredDoc !== null ? e.doc.version : e.snapVersion;

  const editor = new Editor.P215Editor({
    initDoc: initDoc,
    editable: true,
    initialVersion: initVersion,
    sessionClientId: sessionClientId,
    remoteThings: {
      send: (payload: any) => {
        ws.send({ tag: "Updated", contents: payload });
      },
    },
  });
  window.editor = editor;
  window.editorActions = EActions;
  const editorLocation = document.getElementById("editorHolder");
  editor.addEditor(editorLocation);

  let event = new Editor.EditorAttached(editor);
  document.dispatchEvent(event);

  // Apply any OT steps that arrived after the last snapshot but before open.
  // dispatchSteps tags these as remote so they don't trigger SaveDoc.
  if (!restoredDoc && e.pendingSteps.length > 0) {
    editor.dispatchSteps({
      steps: e.pendingSteps,
      clientIds: e.pendingClientIds,
    });
  }

  editor.onUpdate( doc => {
    window.localStorage.setItem(docId + ".doc", JSON.stringify(doc));
    window.localStorage.setItem(localSaveTimeKey, (new Date).toISOString());
    saver.save(doc);
  });

  if (restoredDoc !== null) {
    window.localStorage.setItem(localDoc, JSON.stringify(restoredDoc));
    window.localStorage.setItem(localSaveTimeKey, (new Date).toISOString());
    ws.send({ tag: "SaveDoc", contents: { document: restoredDoc } } as WS.SendSaveDoc);
  }

  ws.addEventListener("DocConfirmed", (ev: WS.DocConfirmedEvent) => {
    editor.confirmSteps(ev.payload);
  });

  ws.addEventListener("DocConflict", (ev: WS.DocConflictEvent) => {
    editor.handleConflict(ev.payload);
  });

  // Receive remote steps from other editors (multi-editor / multi-tab support).
  ws.addEventListener("DocUpdated", (ev: WS.DocUpdatedEvent) => {
    if (ev.update.docId === docId) {
      editor.dispatchSteps(ev.update);
    }
  });

  const studyNameElem = document.getElementById("studyName");
  studyNameElem.addEventListener("input", (ev) => {
    const update : WS.SendUpdateName = {
      tag: "UpdateName",
      contents: studyNameElem.innerText,
    }
    ws.send(update);
  });
  wasInitialized = true;
}

ws.addEventListener("DocOpened", (e : WS.DocOpenedEvent) => {
  if(!wasInitialized) {
    initialize(e)
  } else {
    // WS reconnect: the collab plugin retransmits any inflight steps automatically.
    // No user action needed.
  }
});


function mkSaveObject (ws : WS.MyWebsocket) {
  let isSaving = false;
  let nextSave = null;

  function save (doc: any) {
    const update : WS.SendSaveDoc = {
      tag: "SaveDoc",
      contents: {
        document: doc,
      },
    };
    ws.send(update);
  }

  ws.addEventListener("DocSaved", (ev : WS.DocSavedEvent) => {
    window.localStorage.setItem(lastRemoveSaveTimeKey, ev.time);
    setTimeout(() => {
      if(nextSave === null) {
        isSaving = false;
        document.getElementById("saveThingy").className = "notSaving";
      } else {
        const doc = nextSave;
        nextSave = null;
        save(doc);
      }
    }, 1000);
  });


  return {
    save : (doc : any) => {
      if (!isSaving) {
        isSaving = true;
        document.getElementById("saveThingy").className = "saving";
        save(doc);
      } else {
        nextSave = doc;
      }
    },
  };
};

GS.init(ws);
