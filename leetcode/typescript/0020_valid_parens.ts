function isValid(s: string): boolean {
  const stack: string[] = [];
  for (let i = 0; i < s.length; i++) {
    const c = s[i];
    if (c == '(' || c == '[' || c == '{') {
      stack.push(c);
    } else {
      let cp = stack.pop();
      if (cp == undefined) return false;
      if (cp == '(' && c != ')') return false;
      if (cp == '[' && c != ']') return false;
      if (cp == '{' && c != '}') return false;
    }
  }
  return stack.length == 0;
}
