function dynGrid(m: number, n: number): number[][] {
  let arr = new Array(m);
  for (let i = 0; i < m; i++) {
    arr[i] = new Array(n).fill(0);
  }
  return arr;
}

function findPaths(m: number, n: number, maxMove: number, startRow: number, startColumn: number): number {
  let dyn = dynGrid(m, n);

  for (let i = 1; i <= maxMove; i++) {
    let next = dynGrid(m, n);
    for (let mi = 0; mi < m; mi++) {
      for (let ni = 0; ni < n; ni++) {
        let agg = 0;
        if (mi > 0) {
          agg += dyn[mi - 1][ni];
        } else {
          agg += 1;
        }
        if (ni > 0) {
          agg += dyn[mi][ni - 1];
        } else {
          agg += 1;
        }
        if (mi < m - 1) {
          agg += dyn[mi + 1][ni];
        } else {
          agg += 1;
        }
        if (ni < n - 1) {
          agg += dyn[mi][ni + 1];
        } else {
          agg += 1
        }
        next[mi][ni] = agg;
      }
    }
    dyn = next;
  }

  return dyn[startRow][startColumn] % 1000000007;
}
