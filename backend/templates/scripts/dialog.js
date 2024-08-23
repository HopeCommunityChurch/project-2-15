function toggleDialog(qs) {
  const nav = document.querySelector(qs);
  if(nav.open) {
    nav.close();
  } else {
    nav.show();
  }
}
function toggleModal(qs) {
  const dialog = document.querySelector(qs);
  if(dialog.open) {
    dialog.close();
  } else {
    dialog.showModal();
    const clickFunc = (ev) => {
      var rect = dialog.getBoundingClientRect();
      var isInDialog =
        (rect.top <= ev.clientY
          && ev.clientY <= rect.top + rect.height
          && rect.left <= ev.clientX
          && ev.clientX <= rect.left + rect.width);
      if (!isInDialog) {
        dialog.close();
      }
    };
    const closeFunc = () => {
      dialog.removeEventListener('click', clickFunc);
      dialog.removeEventListener('close', closeFunc);
    };
    dialog.addEventListener('close', closeFunc) ;
    dialog.addEventListener('click', clickFunc);
  }
}

