function digArtifacts(n: number, artifacts: number[][], dig: number[][]): number {
  let uncovered = new Set<number>();
  for (let [r, c] of dig) {
    uncovered.add(c + r * n);
  }

  function canExtract(r1: number, c1: number, r2: number, c2: number): boolean {
    for (let r = r1; r <= r2; r++) {
      for (let c = c1; c <= c2; c++) {
        if (!uncovered.has(c + r * n)) return false;
      }
    }
    return true;
  }

  let extracted = 0;
  for (let [r1, c1, r2, c2] of artifacts) {
    if (canExtract(r1, c1, r2, c2)) extracted++;
  }
  return extracted;
}
