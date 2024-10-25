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

