function distributeCandies(n: number, limit: number): number {
  const a = Math.max(0, n - limit);
  const b = Math.min(n, limit);
  const c = Math.max(0, n - limit * 2);
  const d = Math.min(b, n - limit - 1);

  let combos = 0;
  if (b >= a) {
    combos += (n + 1 - (b + a) / 2) * (b - a + 1);
  }
  if (d >= c) {
    combos += (2 * limit - n + 1 + (d + c) / 2) * (d - c + 1);
  }
  return combos;
}
