// Sets --vh-real CSS variable on <html> to the visual viewport height,
// covering mobile browsers where 100dvh doesn't update for keyboard/address-bar changes.
export function initViewportStabilizer(): void {
  const vv = window.visualViewport;
  if (!vv) return;

  let raf: number | null = null;

  function update() {
    raf = null;
    document.documentElement.style.setProperty('--vh-real', `${vv.height}px`);
  }

  function onResize() {
    if (raf) return;
    raf = requestAnimationFrame(update);
  }

  vv.addEventListener('resize', onResize);
  vv.addEventListener('scroll', onResize);
  update();
}
