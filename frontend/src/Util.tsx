

export function getRandomStr(): string {
  const arrayb = new Uint8Array(10);
  let b = crypto.getRandomValues(arrayb);
  return btoa(b.reduce((a, b) => a + b, ""));
}

