function largestLocal(grid: number[][]): number[][] {
  const height = grid.length;
  const width = grid[0].length;

  let result: number[][] = [];

  for (let x = 1; x < width - 1; x++) {
    result.push([]);
    for (let y = 1; y < height - 1; y++) {
      result[x - 1][y - 1] = Math.max(
        grid[x - 1][y - 1], grid[x - 1][y], grid[x - 1][y + 1],
        grid[x][y - 1], grid[x][y], grid[x][y + 1],
        grid[x + 1][y - 1], grid[x + 1][y], grid[x + 1][y + 1],
      );
    }
  }

  return result;
}
