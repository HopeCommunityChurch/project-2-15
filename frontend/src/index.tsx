
import * as EActions from "./Editor/editorUtils"
import * as Editor from "./Editor/Editor"
import * as WS from "./WebsocketTypes"
import * as T from "./Types"
import {} from "./PreviewScripture"
import {} from "./SidebarSections"


const pathparts = window.location.pathname.split("/");
const docId = pathparts[pathparts.length - 1] as T.DocId;

const ws = new WS.MyWebsocket();
ws.connect();

ws.addEventListener("open", () => {
  ws.openDoc(docId);
});

ws.addEventListener("DocOpened", (e : WS.DocOpenedEvent) => {
  const editor = new Editor.P215Editor({
    initDoc: e.doc,
    editable: true,
    remoteThings: {
      send: (steps: any) => {
        // ws.send({ tag: "Updated", contents: steps });
      },
    },
  });
  window.editor = editor;
  window.editorActions = EActions;
  const editorLocation = document.getElementById("editorHolder");
  editor.addEditor(editorLocation);
  let event = new Editor.EditorAttached(editor);

  document.dispatchEvent(event);
});



