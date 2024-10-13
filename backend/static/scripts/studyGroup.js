function addPeopleInput() {
  const template = document.getElementById("peopleInputTemplate");
  const content = template.content.cloneNode(true);
  const button = content.querySelector("button");
  button.disabled = true;
  button.addEventListener("click", (e) => {
    e.stopPropagation()
    const par = button.parentNode;
    par.parentNode.removeChild(par);
  });
  content.querySelector("input").addEventListener("input", (e) => {
    const value = e.target.value;
    if (value === ""){
      button.disabled = true;
    } else {
      button.disabled = false;
      const createPeoples = document.getElementById("createPeoples");
      const children = Array.from(createPeoples.children)
      if (children[children.length-1] == e.target.parentNode) {
        addPeopleInput();
      }
    }
  });
  document.getElementById("createPeoples").appendChild(content);
}


function studyGroupUpdateSetup() {
  const invitesDiv = document.getElementById("studyGroupInvites");
  const membersDiv = document.getElementById("studyGroupMembers");
  const deletes = document.getElementById("studyGroupDeletes");

  invitesDiv.querySelectorAll("button").forEach( button => {
    button.addEventListener("click", () => {
      const parent = button.parentElement;
      parent.parentElement.removeChild(parent);
      const token = button.getAttribute("data-token");
      const input = document.createElement("input");
      input.name = "delete-invite[]";
      input.value = token;
      deletes.appendChild(input);
    });
  });

  membersDiv.querySelectorAll("button.trash").forEach( button => {
    button.addEventListener("click", () => {
      const parent = button.parentElement;
      parent.parentElement.removeChild(parent);
      const docId = button.getAttribute("data-doc");
      const input = document.createElement("input");
      input.name = "delete-member[]";
      input.value = docId;
      deletes.appendChild(input);
    });
  });
}
