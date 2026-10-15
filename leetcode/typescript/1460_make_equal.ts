function canBeEqual(target: number[], arr: number[]): boolean {
  let counts = new Map<number, number>();
  for (let n of target) {
    const cur = counts.get(n) || 0;
    counts.set(n, cur + 1);
  }

  for (let n of arr) {
    const cur = counts.get(n) || 0;
    counts.set(n, cur - 1);
  }

  for (let n of counts.values()) {
    if (n != 0) return false;
  }
  return true;
}
