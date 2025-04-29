function clearErrors () {
  const errorElems = document.querySelectorAll(".errorText");
  errorElems.forEach( (elem) => {
    elem.parentElement.removeChild(elem);
  });

}
