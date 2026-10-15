struct NumArray {
  sum_tree: Vec<i32>,
  sum_node_count: usize,
}

impl NumArray {
  fn new(nums: Vec<i32>) -> Self {
    let depth = if nums.len() == 1 {
      1usize
    } else {
      f64::from(nums.len() as i32).log2().ceil() as usize
    };
    let sum_node_count: usize = (1 << depth) - 1;
    let mut sum_tree = vec![0i32; sum_node_count];
    sum_tree.extend(nums);

    for i in (0..sum_node_count).rev() {
      let left = sum_tree.get((i << 1) + 1).copied().unwrap_or(0);
      let right = sum_tree.get((i << 1) + 2).copied().unwrap_or(0);
      sum_tree[i] = left + right;
    }

    NumArray {
      sum_tree,
      sum_node_count,
    }
  }

  fn update_sum_tree(&mut self, mut i: usize) {
    while i >= 0 {
      let left = self.sum_tree.get((i << 1) + 1).copied().unwrap_or(0);
      let right = self.sum_tree.get((i << 1) + 2).copied().unwrap_or(0);
      self.sum_tree[i] = left + right;

      if i == 0 {
        break;
      }
      i = (i - 1) >> 1;
    }
  }

  fn update(&mut self, index: i32, val: i32) {
    self.sum_tree[self.sum_node_count + index as usize] = val;
    self.update_sum_tree((self.sum_node_count + index as usize - 1) >> 1);
  }

  fn sum_range(&self, mut left: i32, mut right: i32) -> i32 {
    let mut excluded = 0;

    left += self.sum_node_count as i32;
    right += self.sum_node_count as i32;

    while left != right {
      if (left & 1) == 0 {
        excluded += self.sum_tree.get(left as usize - 1).copied().unwrap_or(0);
      }
      if (right & 1) == 1 {
        excluded += self.sum_tree.get(right as usize + 1).copied().unwrap_or(0);
      }
      left = (left - 1) >> 1;
      right = (right - 1) >> 1;
    }
    self.sum_tree[left as usize] - excluded
  }
}
