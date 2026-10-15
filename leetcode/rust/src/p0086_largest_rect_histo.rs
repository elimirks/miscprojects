impl Solution {
  pub fn largest_rectangle_area(heights: Vec<i32>) -> i32 {
    let mut fit_to_left = vec![0i32; heights.len()];
    let mut fit_to_right = vec![0i32; heights.len()];

    for i in 0..heights.len() {
      let mut j = i as i32 - 1;
      while j >= 0 && heights[j as usize] >= heights[i] {
        fit_to_left[i] += 1 + fit_to_left[j as usize];
        j -= fit_to_left[j as usize] + 1;
      }
    }
    for i in (0..heights.len()).rev() {
      let mut j = i as i32 + 1;
      while j < heights.len() as i32 && heights[j as usize] >= heights[i] {
        fit_to_right[i] += 1 + fit_to_right[j as usize];
        j += fit_to_right[j as usize] + 1;
      }
    }

    heights.into_iter()
      .enumerate()
      .map(|(i, height)| {
        height * (1 + fit_to_left[i] + fit_to_right[i])
      })
      .max()
      .unwrap_or(0)
  }
}
