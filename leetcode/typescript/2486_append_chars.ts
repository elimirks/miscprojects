function appendCharacters(s: string, t: string): number {
  let j = 0;
  for (let i = 0; i < s.length; i++) {
    if (s[i] == t[j]) {
      j++;
      if (j == t.length) return 0;
    }
  }
  return t.length - j;
}
