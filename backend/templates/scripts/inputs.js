window.addEventListener("load", () => {
  document.querySelectorAll("input").forEach( (input) => {
    input.value = input.getAttribute("value");
  });
  document.querySelectorAll("input[type=checkbox]").forEach( (input) => {
    const checked = input.getAttribute("checked");
    input.checked = checked != null;
  });
});

