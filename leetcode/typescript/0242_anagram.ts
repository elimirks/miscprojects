function isAnagram(s: string, t: string): boolean {
  let counts = new Map<string, number>();

  for (let c of s) {
    let cur = counts.get(c) || 0;
    counts.set(c, cur + 1);
  }

  for (let c of t) {
    let cur = counts.get(c) || 0;
    counts.set(c, cur - 1);
  }

  for (let n of counts.values()) {
    if (n != 0) return false;
  }
  return true;
}
