// There are only two possibilities:
// 1. We leave the root node and sum everything up
// 2. We take the root node and don't take one element from each child subtree
function maximumScoreAfterOperations(edges: number[][], values: number[]): number {
  // Step 1: Create an edge map, which is easier to deal with
  let edgeMap: number[][] = new Array();
  for (let i = 0; i < values.length; i++) {
    edgeMap.push([]);
  }
  for (let [a, b] of edges) {
    edgeMap[a].push(b);
    edgeMap[b].push(a);
  }

  // Step 2: Compute sums of subtrees (including the node itself)
  let sums: number[] = [];
  function computeSums(index: number, visited: boolean[]) {
    let sum = values[index];
    for (let child of edgeMap[index]) {
      if (!visited[child]) {
        visited[child] = true;
        computeSums(child, visited);
        sum += sums[child];
      }
    }
    sums[index] = sum;
  }
  let sumsVisited = new Array(values.length).fill(false);
  sumsVisited[0] = true;
  computeSums(0, sumsVisited);

  function solve(index: number, visited: boolean[]): number {
    let hasChildren = edgeMap[index].map((i) => !visited[i]).reduce((a, b) => a || b, false);
    if (!hasChildren) {
      return 0;
    }

    // Case 1: Select the current node and recurse for each child
    let secondSum = values[index];

    for (let child of edgeMap[index]) {
      if (!visited[child]) {
        visited[child] = true;
        secondSum += solve(child, visited);
      }
    }

    // Case 2: Leave the current node and sum the children
    let childSum = sums[index] - values[index];
    return Math.max(childSum, secondSum);
  }

  let visited = new Array(values.length).fill(false);
  visited[0] = true;
  return solve(0, visited);
}
