function isMatch(s: string, p: string): boolean {
  if (p === '') return s === '';
  let toMatch = p[0];
  let repeats = p.length > 1 && p[1] === '*';
  if (s.length == 0 && !repeats) return false;

  if (repeats) {
    let i = 0;
    while (i < s.length && (toMatch === '.' || s[i] === toMatch)) {
      if (isMatch(s.slice(i + 1), p.slice(2))) return true;
      i++;
    }
    return isMatch(s, p.slice(2));
  } else {
    return (toMatch === '.' || s[0] === toMatch) && isMatch(s.slice(1), p.slice(1));
  }
}
