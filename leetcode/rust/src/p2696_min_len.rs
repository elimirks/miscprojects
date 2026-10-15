#[derive(PartialEq)]
enum Begin {
  A,
  C,
}

impl Solution {
  pub fn min_length(s: String) -> i32 {
    let mut removed = 0;
    let mut stack = Vec::with_capacity(s.len());
    for c in s.bytes() {
      if c == 'A' as u8 {
        stack.push(Begin::A);
      } else if c == 'C' as u8 {
        stack.push(Begin::C);
      } else if c == 'B' as u8 {
        if stack.pop() == Some(Begin::A) {
          removed += 2;
        } else {
          stack.clear();
        }
      } else if c == 'D' as u8 {
        if stack.pop() == Some(Begin::C) {
          removed += 2;
        } else {
          stack.clear();
        }
      } else {
        stack.clear();
      }
    }
    return s.len() as i32 - removed;
  }
}
