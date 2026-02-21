"use strict";

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
  content.querySelector("input").addEventListener("keypress", (e) => {
    if(e.key == "Enter") {
      e.stopPropagation();
      e.preventDefault();
      findNextInput(e.target);
    }
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

function nextElement(elem) {
  if(elem.children.length === 0) {
    let parent = elem.parentNode;
    let next = elem.nextElementSibling;
    while(next === null ) {
      next = parent.nextElementSibling
      parent = parent.parentNode;
    }
    return next;
  } else {
    let next = elem.children[0];
    return next;
  }
};

function walkDOMStartingFrom (elem, func) {
  if(elem === null) {
    return null;
  }
  const result = func(elem);
  if (result) {
    return elem;
  }
  const next = nextElement(elem);
  if(next === null) {
    return null;
  }
  return walkDOMStartingFrom(next, func);
}

function findNextInput (elem) {
  const nextInput = walkDOMStartingFrom(nextElement(elem), (e) => {
    return e.tagName.toLowerCase() == "input";
  });
  if(nextInput !== null) {
    nextInput.focus()
  }
}

// Inline invite form in the group study modal
document.addEventListener("htmx:afterSettle", function(evt) {
  const dialog = document.getElementById("groupStudy");
  if (!dialog || !dialog.open) return;
  const emailInput = dialog.querySelector('input[name="email[]"]');
  if (!emailInput) return;
  emailInput.focus();
  if (!emailInput._inviteHandlerAttached) {
    emailInput._inviteHandlerAttached = true;
    emailInput.addEventListener("keydown", function(e) {
      if (e.key === "Enter") {
        e.preventDefault();
        e.stopPropagation();
        htmx.trigger(emailInput.closest("form"), "submit");
      }
    });
  }
});
