// https://leetcode.com/problems/longest-substring-without-repeating-characters/
use std::collections::HashMap;

// Sliding window solution
impl Solution {
  pub fn length_of_longest_substring(s: String) -> i32 {
    let mut cur_len: usize   = 0;
    let mut cur_index: usize = 0;
    let mut current_chars = HashMap::<char, usize>::new();
    let chars = s.chars().collect::<Vec<char>>();

    while cur_index + cur_len < chars.len() {
      // Check if the current chars are valid
      let is_valid_window = current_chars.values().all(|n| *n <= 1);

      let tail_c = chars.get(cur_index + cur_len).unwrap();
      if is_valid_window && *current_chars.get(tail_c).unwrap_or(&0) == 0 {
        // Grow the window
        current_chars.insert(*tail_c, 1);
        cur_len += 1;
      } else {
        // Slide the window
        *current_chars.entry(*tail_c).or_insert(0) += 1;
        let head_c = chars.get(cur_index).unwrap();
        *current_chars.entry(*head_c).or_insert(1) -= 1;
        cur_index += 1;
      }
    }
    cur_len as i32
  }
}
