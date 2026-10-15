enum Begin {
  A,
  C,
}

function minLength(s: string): number {
  let removed = 0;
  let stack: Begin[] = [];
  for (let i = 0; i < s.length; i++) {
    if (s[i] == "A") {
      stack.push(Begin.A);
    } else if (s[i] == "C") {
      stack.push(Begin.C);
    } else if (s[i] == "B") {
      if (stack.pop() == Begin.A) {
        removed += 2;
      } else {
        stack.length = 0;
      }
    } else if (s[i] == "D") {
      if (stack.pop() == Begin.C) {
        removed += 2;
      } else {
        stack.length = 0;
      }
    } else {
      stack.length = 0;
    }
  }
  return s.length - removed;
}
