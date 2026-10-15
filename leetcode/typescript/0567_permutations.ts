function checkInclusionOld(needle: string, haystack: string): boolean {
  let len = needle.length;
  if (len == 0) return true;

  // Counts of each needle character
  let needleCounts = new Map<string, number>();
  for (let i = 0; i < needle.length; i++) {
    let cur = needleCounts.get(needle[i]) || 0;
    needleCounts.set(needle[i], cur + 1);
  }

  // Counts of applicable chars in the sliding window
  let windowCounts = new Map<string, number>();
  // Initialize window counts
  for (let i = 0; i < Math.min(len, haystack.length); i++) {
    let cur = windowCounts.get(haystack[i]) || 0;
    windowCounts.set(haystack[i], cur + 1);
  }
  let headChar = haystack[0];

  function windowMatches(): boolean {
    for (let [c, count] of needleCounts) {
      if (windowCounts.get(c) != count) return false;
    }
    return true;
  }

  let idx = 0;

  while (!windowMatches() && idx + len < haystack.length) {
    let cur = windowCounts.get(headChar)!;
    windowCounts.set(headChar, cur - 1);
    idx++;
    headChar = haystack[idx];
    let tailChar = haystack[idx + len - 1];
    cur = windowCounts.get(tailChar) || 0;
    windowCounts.set(tailChar, cur + 1);
  }

  return windowMatches();
}

function checkInclusionDeltaCounts(needle: string, haystack: string): boolean {
  if (needle.length > haystack.length) return false;

  // Counts of each needle character
  let deltaCounts: number[] = new Array(26).fill(0);
  for (let i = 0; i < needle.length; i++) {
    deltaCounts[needle.charCodeAt(i) - 97]--;
    deltaCounts[haystack.charCodeAt(i) - 97]++;
  }

  function windowMatches(): boolean {
    for (let i = 0; i < deltaCounts.length; i++) {
      if (deltaCounts[i] != 0) return false;
    }
    return true;
  }
  if (windowMatches()) return true;

  // i points at the next char in the window
  for (let i = needle.length; i < haystack.length; i++) {
    let oldChar = haystack.charCodeAt(i - needle.length) - 97;
    deltaCounts[oldChar]--;

    let nextChar = haystack.charCodeAt(i) - 97;
    deltaCounts[nextChar]++;

    if (deltaCounts[nextChar] == 0 && deltaCounts[oldChar] == 0) {
      if (windowMatches()) return true;
    }
  }

  return false;
}
