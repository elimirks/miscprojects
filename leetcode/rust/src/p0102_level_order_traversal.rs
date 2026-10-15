#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
  pub val: i32,
  pub left: Option<Rc<RefCell<TreeNode>>>,
  pub right: Option<Rc<RefCell<TreeNode>>>,
}

impl TreeNode {
  #[inline]
  pub fn new(val: i32) -> Self {
    TreeNode {
      val,
      left: None,
      right: None
    }
  }
}

use std::collections::VecDeque;
use std::rc::Rc;
use std::cell::RefCell;

impl Solution {
    pub fn level_order(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<Vec<i32>> {
      let Some(root) = root else {
        return vec![];
      };

      let mut queue = VecDeque::new();
      queue.push_back((root.clone(), 0));

      let mut result = vec![];
      while let Some((node, depth)) = queue.pop_front() {
        if result.len() < depth + 1 {
          result.push(vec![]);
        }
        result[depth].push(node.borrow().val);

        if let Some(ref left) = node.borrow().left {
          queue.push_back((left.clone(), depth + 1))
        }
        if let Some(ref right) = node.borrow().right {
          queue.push_back((right.clone(), depth + 1))
        }
      }
      result
    }
}
