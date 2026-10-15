function minEdgeReversalsBfs(n: number, edges: number[][]): number[] {
  let forwardEdgeMap: number[][] = [];
  let backwardEdgeMap: number[][] = [];
  for (let i = 0; i < n; i++) {
    forwardEdgeMap.push([]);
    backwardEdgeMap.push([]);
  }

  for (let [a, b] of edges) {
    forwardEdgeMap[a].push(b);
    backwardEdgeMap[b].push(a);
  }

  // Step 1: Find reachable nodes for each starting node
  let alwaysReachable: Set<number>[] = [];
  function computeAlwaysReachable(index: number) {
    if (alwaysReachable[index] !== undefined) {
      return;
    }

    let reachable = new Set<number>([index]);
    for (let next of forwardEdgeMap[index]) {
      computeAlwaysReachable(next);
      for (let x of alwaysReachable[next]) {
        reachable.add(x);
      }
    }
    alwaysReachable[index] = reachable;
  }
  for (let i = 0; i < n; i++) {
    computeAlwaysReachable(i);
  }

  // Step 2: BFS from the reachable nodes to find the number of edge reversals
  let results: number[] = [];
  for (let i = 0; i < n; i++) {
    let reversals = 0;
    let reachable = new Set(alwaysReachable[i]);

    let queue: number[] = [];
    for (let index of reachable) {
      queue.push(index);
    }

    while (reachable.size != n) {
      // Guaranteed to exist if we haven't hit all reachable nodes yet
      let index = queue.shift()!;
      for (let child of backwardEdgeMap[index]) {
        if (!reachable.has(child)) {
          for (let x of alwaysReachable[child]) {
            if (!reachable.has(x)) {
              reachable.add(x);
              queue.push(x);
            }
          }
          reversals++;
        }
      }
    }

    results.push(reversals);
  }
  return results;
}

function minEdgeReversals(n: number, edges: number[][]): number[] {
  let forwardEdgeMap: number[][] = [];
  let backwardEdgeMap: number[][] = [];
  for (let i = 0; i < n; i++) {
    forwardEdgeMap.push([]);
    backwardEdgeMap.push([]);
  }
  for (let [a, b] of edges) {
    forwardEdgeMap[a].push(b);
    backwardEdgeMap[b].push(a);
  }

  // Step 1: DFS to compute edge reversals for each subtree
  let subtreeReversals: number[] = [];

  function subtreeDfs(index: number, visited: Set<number>) {
    let reversals = 0;

    for (let forwardChild of forwardEdgeMap[index]) {
      if (!visited.has(forwardChild)) {
        visited.add(forwardChild);
        subtreeDfs(forwardChild, visited);
        reversals += subtreeReversals[forwardChild];
      }
    }
    for (let backwardChild of backwardEdgeMap[index]) {
      if (!visited.has(backwardChild)) {
        visited.add(backwardChild);
        subtreeDfs(backwardChild, visited);
        reversals += 1 + subtreeReversals[backwardChild];
      }
    }

    subtreeReversals[index] = reversals;
  }
  subtreeDfs(0, new Set([0]));

  console.log(subtreeReversals);

  // Step 2: Use the DP result to calculate the answer in O(n) time

  return [];
}
