// Naive is to use BFS
// But we know we can only move in two directions - down and right
// So can we optimize the storage at all?
function minimumVisitedCellsBfs(grid: number[][]): number {
  const width = grid.length;
  const height = grid[0].length;

  let queue: number[] = [0];
  let pathLens: number[] = [1];
  let visited = new Set<number>([0]);
  while (queue.length > 0) {
    let next = queue.shift()!;
    let nextPathLen = pathLens.shift()!;

    const x = next % width;
    const y = Math.floor(next / width);
    const n = grid[x][y];

    if (x == width - 1 && y == height - 1) {
      return nextPathLen;
    }

    for (let xi = x + 1; xi <= Math.min(width - 1, x + n); xi++) {
      const idx = y * width + xi;
      if (!visited.has(idx)) {
        visited.add(idx);
        queue.push(idx);
        pathLens.push(nextPathLen + 1);
      }
    }
    for (let yi = y + 1; yi <= Math.min(height - 1, y + n); yi++) {
      const idx = yi * width + x;
      if (!visited.has(idx)) {
        visited.add(idx);
        queue.push(idx);
        pathLens.push(nextPathLen + 1);
      }
    }
  }
  return -1;
}

function minimumVisitedCells(grid: number[][]): number {
  const width = grid.length;
  const height = grid[0].length;

  grid[width - 1][height - 1] = 1;

  // Fill in the last row for DP
  for (let x = width - 2; x >= 0; x--) {
    const n = grid[x][height - 1];
    let minVisited = Infinity;

    console.log("foo:", x + 1, width - 1, x + n, Math.min(width - 1, x + n));
    for (let xi = x + 1; xi <= Math.min(width - 1, x + n); xi++) {
      console.log(x, xi);
      const next = grid[xi][height - 1];
      if (next != -1) {
        minVisited = Math.min(minVisited, next);
      }
    }

    if (minVisited == Infinity || minVisited == -1) {
      grid[x][height - 1] = -1;
    } else {
      grid[x][height - 1] = 1 + minVisited;
    }
  }

  console.log(grid);

  return -1;
}
