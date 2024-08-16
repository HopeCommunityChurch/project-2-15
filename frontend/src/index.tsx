
import * as EActions from "./Editor/editorUtils"
import * as Editor from "./Editor/Editor"
import * as WS from "./WebsocketTypes"
import * as T from "./Types"
import {} from "./PreviewScripture"
import {} from "./SidebarSections"
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


ws.addEventListener("open", () => {
  ws.sendStrRaw(computerId);
  ws.openDoc(docId);
});

const localSaveTimeKey = docId +  ".time"
const lastRemoveSaveTimeKey = docId +  ".remoteSaveTime"
const localDoc = docId + ".doc";


function checkLastUpdate (doc : T.DocRaw) : any {
  if(doc.lastUpdate == null) {
    console.log("using remote doc");
    return doc.document;
  }
  if (doc.lastUpdate.computerId == computerId) {
    console.log("using local storage");
    const result = window.localStorage.getItem(localDoc);
    return JSON.parse(result);
  } else {
    // We need to make this nicer in the where if a local update hasn't been
    // saved we can present a choise for the user.
    console.error("using remote doc");
    return doc.document;
  }
}

let wasInitialized = false;

function initialize (e : WS.DocOpenedEvent) {
  const saver = mkSaveObject(ws);

  const doc = checkLastUpdate(e.doc)

  const editor = new Editor.P215Editor({
    initDoc: doc,
    editable: true,
    remoteThings: {
      send: (steps: any) => {
        ws.send({ tag: "Updated", contents: steps });
      },
    },
  });
  window.editor = editor;
  window.editorActions = EActions;
  const editorLocation = document.getElementById("editorHolder");
  editor.addEditor(editorLocation);

  let event = new Editor.EditorAttached(editor);
  document.dispatchEvent(event);

  editor.onUpdate( doc => {
    window.localStorage.setItem(docId + ".doc", JSON.stringify(doc));
    window.localStorage.setItem(localSaveTimeKey, (new Date).toISOString());
    saver.save(doc);
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

    if (e.doc.lastUpdate.computerId != computerId) {
      alert("You disconnected and there was an update since your last change. Updating this document will override those changes.");
    }
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
