function lengthOfLongestSubstring(s: string): number {
  let first = 0;
  let len = 0;

  while (first + len < s.length) {
    const sub = s.slice(first, first + len + 1);
    const set = new Set(sub);
    if (set.size === sub.length) {
      len++;
    } else {
      first++;
    }
  }
  return len;
}
