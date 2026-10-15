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

  // Step 2: build a graph connecting adjacent words
  let edges: Map<string, string[]> = new Map();
  for (let word of wordList) {
    edges.set(word, trie.getAdjacent(word));
  }

  // Step 3: BFS to find shortest distances
  // Stores the paths taken to each word
  // TODO: Store paths separately so we're not cloning the array so often
  let queue: [string, string[]][] = [[beginWord, [beginWord]]];
  let visited = new Set<string>([]);

  let endDepth: number | null = null;

  let results: string[][] = [];
  while (queue.length > 0) {
    const [v, path] = queue.shift()!;

    if (endDepth !== null && path.length > endDepth) {
      continue;
    }

    if (v === endWord) {
      endDepth = path.length;
      results.push(path);
    } else {
      for (let next of (edges.get(v) || [])) {
        if (!path.includes(next)) {
          queue.push([next, [...path, next]]);
        }
      }
    }
  }
  return results;
}
