function arrayRankTransform(arr: number[]): number[] {
  let sorted = [...arr].sort((a, b) => a - b);
  let ranks = new Map<number, number>();

  let cur = 1;
  for (let n of sorted) {
    if (!ranks.has(n)) {
      ranks.set(n, cur);
      cur++;
    }
  }
  return arr.map((n) => ranks.get(n)!);
}
