impl Solution {
  pub fn is_isomorphic(s: String, t: String) -> bool {
    let mut mapping = [u8::MAX; 128];
    let mut is_mapped_to = [false; 128];

    for (s_code, t_code) in s.into_bytes().into_iter().zip(t.into_bytes().into_iter()) {
      let code_map = mapping[s_code as usize];

      if code_map != u8::MAX {
        if t_code != code_map {
          return false;
        }
      } else {
        if is_mapped_to[t_code as usize] {
          return false;
        }
        mapping[s_code as usize] = t_code;
        is_mapped_to[t_code as usize] = true;
      }
    }
    true
  }
}
