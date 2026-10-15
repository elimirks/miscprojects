function dfs(board: string[][], word: string, x: number, y: number, visited: Set<number>): boolean {
  if (word.length == 0) return true;
  if (x < 0 || y < 0 || x >= board.length || y >= board[0].length) return false;

  let idx = x + y * board.length;
  if (visited.has(idx)) return false;

  if (word.length == 1) return board[x][y] == word[0];
  if (word[0] != board[x][y]) return false;

  let sub = word.substring(1);

  visited.add(idx);
  if (dfs(board, sub, x - 1, y, visited)) return true;
  if (dfs(board, sub, x + 1, y, visited)) return true;
  if (dfs(board, sub, x, y - 1, visited)) return true;
  if (dfs(board, sub, x, y + 1, visited)) return true;
  visited.delete(idx);

  return false;
}

function exist(board: string[][], word: string): boolean {
  let visited = new Set<number>();
  for (let x = 0; x < board.length; x++) {
    for (let y = 0; y < board[0].length; y++) {
      if (dfs(board, word, x, y, visited)) return true;
    }
  }
  return false;
}
