function isPrime(n: number) {
  if (n <= 1) return false;
  if (n <= 3) return true;

  // This is checked so that we can skip
  // middle five numbers in below loop
  if (n % 2 == 0 || n % 3 == 0) return false;

  for (let i = 5; i * i <= n; i = i + 6) {
    if (n % i == 0 || n % (i + 2) == 0) return false;
  }
  return true;
}

class Tree {
  value: number;
  isPrime: boolean;
  children: Tree[];

  constructor(value: number) {
    this.value = value;
    this.isPrime = isPrime(value);
    this.children = [];
  }
}

class GraphNode {
  value: number;
  edges: GraphNode[];

  constructor(value: number) {
    this.value = value;
    this.edges = [];
  }
}

/// The first node is always the root
function createTree(n: number, edges: number[][]): Tree[] {
  let treeNodes: Tree[] = [];
  let graphNodes: GraphNode[] = [];
  for (let i = 1; i <= n; i++) {
    graphNodes.push(new GraphNode(i));
    treeNodes.push(new Tree(i));
  }
  for (let edge of edges) {
    let [i, j] = edge;
    graphNodes[i - 1].edges.push(graphNodes[j - 1]);
    graphNodes[j - 1].edges.push(graphNodes[i - 1]);
  }

  // DFS from an arbitrary GraphNode to construct the directed Tree
  let stack: GraphNode[] = [graphNodes[0]];
  let visited = new Set<number>();
  visited.add(1);
  while (stack.length > 0) {
    let node = stack.pop()!;

    for (let child of node.edges) {
      if (!visited.has(child.value)) {
        visited.add(child.value);
        treeNodes[node.value - 1].children.push(treeNodes[child.value - 1]);
        stack.push(child);
      }
    }
  }
  return treeNodes;
}

interface CountResult {
  pathsWithOnePrime: number;
  pathsWithZeroPrimes: number;
  validPaths: number;
}

function countTreePaths(head: Tree): CountResult {
  let result: CountResult = {
    pathsWithOnePrime: 0,
    pathsWithZeroPrimes: 0,
    validPaths: 0,
  };

  if (head.isPrime) {
    result.pathsWithOnePrime = 1;
  } else {
    result.pathsWithZeroPrimes = 1;
  }

  let previousChildResults: CountResult = {
    pathsWithOnePrime: 0,
    pathsWithZeroPrimes: 0,
    validPaths: 0,
  };

  for (let child of head.children) {
    let childResult = countTreePaths(child);
    result.validPaths += childResult.validPaths;

    if (head.isPrime) {
      result.pathsWithOnePrime += childResult.pathsWithZeroPrimes;
      // Path from this node to each dangling non prime in the child node
      result.validPaths += childResult.pathsWithZeroPrimes;
      // Paths connecting dangling non primes through this head into other children
      result.validPaths += childResult.pathsWithZeroPrimes * previousChildResults.pathsWithZeroPrimes;
    } else {
      result.pathsWithOnePrime += childResult.pathsWithOnePrime;
      result.pathsWithZeroPrimes += childResult.pathsWithZeroPrimes;

      result.validPaths += childResult.pathsWithOnePrime;
      result.validPaths += childResult.pathsWithZeroPrimes * previousChildResults.pathsWithOnePrime;
      result.validPaths += childResult.pathsWithOnePrime * previousChildResults.pathsWithZeroPrimes;
    }

    previousChildResults.pathsWithOnePrime += childResult.pathsWithOnePrime;
    previousChildResults.pathsWithZeroPrimes += childResult.pathsWithZeroPrimes;
    previousChildResults.validPaths += childResult.validPaths;
  }

  return result;
}

function countPaths(n: number, edges: number[][]): number {
  let treeNodes = createTree(n, edges);
  return countTreePaths(treeNodes[0]).validPaths;
}
