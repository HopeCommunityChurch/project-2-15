import * as T from "Types";
import * as Network from "Util/Network";
import * as EActions from "./Editor/editorUtils"

function mkVerse (verse : T.Verse) : DocumentFragment {
  const frag = new DocumentFragment();

  const verseNum = document.createElement("span");
  verseNum.className = "verseNum";
  frag.appendChild(verseNum);
  if(verse.verse === 1) {
    verseNum.innerText = verse.chapter + ":" + verse.verse ;
  } else {
    verseNum.innerText = verse.verse + "";
  }
  const passage = document.createElement("span");
  passage.className = "passage";
  passage.innerText = verse.passage;
  frag.appendChild(passage);

  return frag;
};


function mkVerses (verses : Array<T.Verse>) : DocumentFragment {
  const frag = new DocumentFragment();
  for(const verse of verses) {
    frag.appendChild(mkVerse(verse))
  }
  return frag;
}

function populatePreview (passage : T.ESVResponse) {
  const preview = document.getElementById("previewScripture");
  preview.style.display = "block";

  const header = preview.querySelector("header");
  header.innerHTML = passage.canonical;

  const section = preview.querySelector("section");
  section.innerHTML = "";

  section.appendChild(mkVerses(passage.passage));

}

function getPassage (p : string) : Promise<Network.SimpleNetworkState<T.ESVResponse>>{
  const url = "/bible/esv?q="+encodeURIComponent(p);
  return Network.request(url);
}

function previewScripture(ev : SubmitEvent) {
  ev.preventDefault();
  ev.stopPropagation();
  const input = document.getElementById("addScripture") as HTMLInputElement;
  const passageRef = input.value;
  getPassage(passageRef).then( (result) => {
    switch(result.state) {
      case "error":
        alert(result.body);
        break;
      case "success":
        populatePreview(result.body);
        const addButton = document.getElementById("addScriptureButton") as HTMLInputElement;
        addButton.onclick = (ev) => {
          ev.preventDefault();
          const dialog = document.getElementById("addScripturePopup") as HTMLDialogElement;
          dialog.close();
          const ref = result.body.canonical
          const passage = result.body.passage
          const addVerseDispatch = EActions.addVerse(ref, passage);
          window.editor.applyDispatch(addVerseDispatch);
        };
        addButton.focus();
        break;
    }

  });

}

window.previewScripture = previewScripture;
