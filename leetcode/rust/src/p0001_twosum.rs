// https://leetcode.com/problems/two-sum/
use std::collections::HashMap;

// O(n) complexity, O(n) memory
impl Solution {
  pub fn two_sum(nums: Vec<i32>, target: i32) -> Vec<i32> {
    let mut values = HashMap::<i32, usize>::new();
    for (index, &n) in nums.iter().enumerate() {
      let diff = target - n;
      if let Some(&other_index) = values.get(&diff) {
        return vec![index as i32, other_index as i32];
      }
      values.insert(n, index);
    }
    unreachable!()
  }
}

// O(n^2) complexity, O(1) memory
impl Solution {
  pub fn two_sum(nums: Vec<i32>, target: i32) -> Vec<i32> {
    for index1 in 0..nums.len() {
      for index2 in (index1 + 1)..nums.len() {
        if nums[index1] + nums[index2] == target {
          return vec![index1 as i32, index2 as i32];
        }
      }
    }
    unreachable!()
  }
}
