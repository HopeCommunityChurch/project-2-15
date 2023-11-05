
console.log("test");
document.getElementById("profile_button").addEventListener("click",  (e) => {
  e.stopPropagation();
  const elem = document.getElementById("profile_dropdown");
  const isShown = elem.classList.contains("show");

  function handleClickoutSide (event) {
    const target = event.target;
    if(elem.contains(target)) {
      return;
    }
    removeShow();
  }

  function removeShow () {
    elem.classList.remove("show")
    window.removeEventListener("click", handleClickoutSide);
  }
  if (isShown) {
    removeShow();
    return;
  }

  window.addEventListener("click", handleClickoutSide);
  elem.classList.add("show");
  return;
});
