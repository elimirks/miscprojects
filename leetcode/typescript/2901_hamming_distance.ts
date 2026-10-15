function isValidAdjacent(first: number, second: number, words: string[], groups: number[]): boolean {
  if (groups[first] == groups[second]) return false;
  if (words[first].length != words[second].length) return false;

  let distance = 0;
  for (let i = 0; i < words[first].length; i++) {
    if (words[first].charCodeAt(i) != words[second].charCodeAt(i)) distance++;
    if (distance > 1) return false;
  }
  return distance == 1;
}

function calculateLongest(index: number, edges: number[][], longestSubsequences: number[][]) {
  let longestChildSubsequence: number[] = [];
  for (let child of edges[index]) {
    if (longestSubsequences[child].length > longestChildSubsequence.length) {
      longestChildSubsequence = longestSubsequences[child];
    }
  }
  longestSubsequences[index] = [index, ...longestChildSubsequence];
}

function getWordsInLongestSubsequence(words: string[], groups: number[]): string[] {
  let edges: number[][] = [];

  // Subsequences must be selected in order, so we only have to set forward edges
  for (let i = 0; i < words.length; i++) {
    edges[i] = [];
    for (let j = i + 1; j < words.length; j++) {
      if (isValidAdjacent(i, j, words, groups)) {
        edges[i].push(j);
      }
    }
  }

  let longestSubsequences: number[][] = [];
  for (let i = edges.length - 1; i >= 0; i--) {
    calculateLongest(i, edges, longestSubsequences);
  }

  let answer: number[] = [];
  for (let sub of longestSubsequences) {
    if (sub.length > answer.length) {
      answer = sub;
    }
  }
  return answer.map((index) => words[index]);
}
