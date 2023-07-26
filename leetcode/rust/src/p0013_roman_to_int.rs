// https://leetcode.com/problems/roman-to-integer/
impl Solution {
  pub fn roman_to_int(s: String) -> i32 {
    let chars = s.chars().collect::<Vec<_>>();

    let mut result = 0;
    let mut prev = 0;

    for i in 0..chars.len() {
      let cur = match chars[i] {
        'M' => 1000,
        'D' => 500,
        'C' => 100,
        'L' => 50,
        'X' => 10,
        'V' => 5,
        'I' => 1,
        c => panic!("Invalid numeral: {c}")
      };
      if cur > prev && prev != 0 {
        result += cur - 2 * prev;
        prev = 0;
      } else {
        result += cur;
        prev = cur;
      }
    }
    result
  }
}
