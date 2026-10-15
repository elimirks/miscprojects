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

interface SubSequenceList {
  index: number,
  length: number,
  next: SubSequenceList | null,
}

function calculateLongest(index: number, edges: number[][], longestSubsequences: SubSequenceList[]) {
  let longestChild: SubSequenceList | null = null;
  for (let child of edges[index]) {
    if (longestChild == null) {
      longestChild = longestSubsequences[child];
    } else if (longestSubsequences[child].length > longestChild.length) {
      longestChild = longestSubsequences[child];
    }
  }
  longestSubsequences[index] = {
    index,
    length: (longestChild?.length || 0) + 1,
    next: longestChild,
  };
}

function listToWords(ssList: SubSequenceList, words: string[]): string[] {
  let result: string[] = [];
  let head: SubSequenceList | null = ssList;
  while (head !== null) {
    result.push(words[head.index]);
    head = head.next;
  }
  return result;
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

  let longestSubsequences: SubSequenceList[] = [];
  for (let i = edges.length - 1; i >= 0; i--) {
    calculateLongest(i, edges, longestSubsequences);
  }

  let answer: SubSequenceList = longestSubsequences[0];
  for (let sub of longestSubsequences) {
    if (sub.length > answer.length) {
      answer = sub;
    }
  }
  return listToWords(answer, words);
}
