function isIsomorphic(s: string, t: string): boolean {
  if (s.length != t.length) return false;

  let mapping = new Map<number, number>();
  let isMappedTo = new Set<number>();

  for (let i = 0; i < s.length; i++) {
    let sCode = s.charCodeAt(i);
    let tCode = t.charCodeAt(i);
    let codeMap = mapping.get(sCode);

    if (codeMap) {
      if (tCode != codeMap) return false;
    } else {
      if (isMappedTo.has(tCode)) return false;
      mapping.set(sCode, tCode);
      isMappedTo.add(tCode);
    }
  }
  return true;
}
