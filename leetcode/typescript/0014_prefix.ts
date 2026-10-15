function longestCommonPrefix(strs: string[]): string {
  let len = 0;
  let maxLen = strs.map((s) => s.length).reduce((a, b) => Math.min(a, b), strs[0].length);

  for (let i = 0; i < maxLen; i++) {
    for (let str of strs) {
      if (str[i] != strs[0][i]) {
        return strs[0].substring(0, len);
      }
    }
    len++;
  }
  return strs[0].substring(0, len);
}
