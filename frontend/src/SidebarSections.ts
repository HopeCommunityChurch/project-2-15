import { Node } from "prosemirror-model";
import * as Editor from "./Editor/Editor"
import * as EditorUtil from "./Editor/editorUtils"

document.addEventListener("editorAttached", (ev : Editor.EditorAttached) => {
  const editor = ev.editor;
  const container = document.getElementById("sectionContainer");
  const doc = editor.view.state.doc;
  initialSetup(editor, container, doc);
  editor.onUpdate( (doc) => {
    if (doc.childCount > container.children.length) {
      for (let i = 0; i < doc.childCount - container.children.length; i++) {
        container.appendChild(createSectionHeader(editor, container.children.length+i));
      }
    } else if (doc.childCount < container.children.length) {
      for (let i = 0; i < container.children.length - doc.childCount; i++) {
        container.children.item(container.children.length-1-i).remove();
      }
    }
    updateSectionNames(container, doc);
  });
});


function updateSectionNames (container: HTMLElement, doc : Node) {
  doc.forEach( (node, _, index) => {
    const sectionHeader = node.child(0);
    const sectionText = sectionHeader.child(0);
    const oldTxt = container.children.item(index).innerHTML;
    if (oldTxt != sectionText.text) {
      container.children.item(index).querySelector("span").innerHTML = sectionText.text;
    }
  });
}

function initialSetup(editor : Editor.P215Editor, container: HTMLElement, doc : Node) {
  doc.forEach( (node, _, index) => {
    const section = createSectionHeader(editor, index);
    section.querySelector("span").innerHTML = node.child(0).child(0).text;
    container.appendChild(section);
  });
}

function createSectionHeader(editor : Editor.P215Editor, index : number) {
  const section = document.createElement("div");
  section.className = "section";
  section.addEventListener("click", () => {
    editor.scrollTo(index);
  });

  const sectionText = document.createElement("span");
  section.appendChild(sectionText);

  const remove = document.createElement("div");
  remove.innerHTML = "x";
  const img = document.createElement("img");
  img.src = "/static/img/x.svg";
  remove.className = "remove";
  remove.addEventListener("click", (e) => {
    e.stopPropagation();
    editor.applyDispatch(EditorUtil.deleteSection(index));
  });
  section.appendChild(remove);

  return section;
};
