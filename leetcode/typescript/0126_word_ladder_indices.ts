class Trie {
  values: Map<string, Trie>;

  constructor() {
    this.values = new Map();
  }

  insert(word: string) {
    if (word.length == 0) return;
    let sub = this.values.get(word[0]) || new Trie();
    sub.insert(word.slice(1));
    this.values.set(word[0], sub);
  }

  /// Returns all adjacent words (not including itself)
  getAdjacent(word: string, hadPivot: boolean = false): string[] {
    if (word.length == 0) {
      return hadPivot ? [""] : [];
    }
    let subWord = word.slice(1);

    if (hadPivot) {
      let subTrie = this.values.get(word[0]);
      if (!subTrie) return [];

      let res: string[] = [];
      for (let adjacentSub of subTrie.getAdjacent(subWord, true)) {
        res.push(word[0] + adjacentSub);
      }
      return res;
    } else {
      let res: string[] = [];
      for (let [c, subTrie] of this.values) {
        const isPivot = c != word[0];
        for (let subAdjacent of subTrie.getAdjacent(subWord, isPivot)) {
          res.push(c + subAdjacent);
        }
      }
      return res;
    }
  }

  print(depth: number = 0) {
    let indentation = "";
    for (let i = 0; i < depth; i++) {
      indentation = indentation + "  ";
    }
    for (let [c, subTrie] of this.values) {
      console.log(indentation + c);
      subTrie.print(depth + 1);
    }
  }
}

// Linked list for more efficient path saving
interface FullListNode {
  value: number,
  next: SinglyListNode,
}

type SinglyListNode = FullListNode | null;

function listContains(node: SinglyListNode, entry: number): boolean {
  while (node != null) {
    if (node.value == entry) return true;
    node = node.next;
  }
  return false;
}

function listToArray(node: SinglyListNode): number[] {
  let result: number[] = [];
  while (node !== null) {
    result.push(node.value);
    node = node.next;
  }
  return result;
}

function findLadders(beginWord: string, endWord: string, wordList: string[]): string[][] {
  // Step 0: Handle edge cases
  if (beginWord === endWord) {
    return [[beginWord]];
  }

  if (!wordList.includes(beginWord)) {
    // For some god forsaken reason, they don't include it all the time
    wordList.push(beginWord);
  }

  // Step 1: Use a Trie to quickly find adjacent words
  let trie = new Trie();
  for (let word of wordList) {
    trie.insert(word);
  }
  console.log("Constructed Trie"); 

  // Step 2: build a graph connecting adjacent words
  // Maps indices
  let edges: Map<number, number[]> = new Map();
  for (let word of wordList) {
    const wordIndex = wordList.indexOf(word);
    const adjacentIndices = trie.getAdjacent(word).map((adjacent) => wordList.indexOf(adjacent));
    edges.set(wordIndex, adjacentIndices);
  }
  console.log("Constructed graph"); 

  // Step 3: BFS to find shortest distance
  const beginIndex = wordList.indexOf(beginWord);
  const endIndex = wordList.indexOf(endWord);

  let queue: [number, number, SinglyListNode][] = [[beginIndex, 0, {
    value: beginIndex,
    next: null,
  }]];

  let endDepth: number | null = null;
  let resultIndices: SinglyListNode[] = [];

  while (queue.length > 0) {
    const [v, depth, path] = queue.shift()!;
    if (endDepth !== null && depth > endDepth) {
      continue;
    }

    if (v === endIndex) {
      endDepth = depth;
      resultIndices.push(path);
    } else {
      let nextEdges = edges.get(v);
      if (nextEdges === undefined) continue;
      for (let next of nextEdges) {
        if (!listContains(path, next)) {
          queue.push([next, depth + 1, {
            value: next,
            next: path,
          }]);
        }
      }
    }
  }
  console.log("Called BFS"); 


  return resultIndices.map((head) => {
    return listToArray(head).reverse().map((index) => {
      return wordList[index];
    });
  });

  // // Step 4: DFS to collect results
  // function dfs(depth: number, visited: Set<number>, current: number): number[][] | null {
  //   if (current === endIndex) return [[current]];
  //   if (depth < 1) return null;
  //
  //   let nextEdges = edges.get(current);
  //   if (nextEdges === undefined) return [];
  //
  //   let results: number[][] = [];
  //
  //   visited.add(current);
  //
  //   for (let next of nextEdges) {
  //     if (!visited.has(next)) {
  //       let subPaths = dfs(depth - 1, visited, next);
  //       if (subPaths === null) continue;
  //       for (let sub of subPaths) {
  //         results.push([current, ...sub]);
  //       }
  //     }
  //   }
  //   visited.delete(current);
  //   return results
  // }
  // if (endDepth == null) return [];
  //
  // let resultIndices = dfs(endDepth, new Set(), beginIndex);
  // if (resultIndices === null) return [];
  // console.log("Called DFS"); 

  // return resultIndices.map((indices) => {
  //   return indices.map((index) => wordList[index]);
  // });
}
