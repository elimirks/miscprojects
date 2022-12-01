// Definition for singly-linked list.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
  pub val: i32,
  pub next: Option<Box<ListNode>>
}

impl ListNode {
  #[inline]
  fn new(val: i32) -> Self {
    ListNode {
      next: None,
      val
    }
  }
}

impl Solution {
  pub fn add_two_numbers(l1: Option<Box<ListNode>>, l2: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
    solve(l1, l2, 0)
  }
}

fn solve(lhs: Option<Box<ListNode>>, rhs: Option<Box<ListNode>>, carry: i32) -> Option<Box<ListNode>> {
  match (lhs, rhs) {
    (None, None) => {
      if carry != 0 {
        Some(Box::new(ListNode::new(carry)))
      } else {
        None
      }
    },
    (None, Some(rhs)) => {
      let s = rhs.val + carry;
      new_node(s % 10, solve(None, rhs.next, s / 10))
    },
    (Some(lhs), None) => {
      let s = lhs.val + carry;
      new_node(s % 10, solve(lhs.next, None, s / 10))
    },
    (Some(lhs), Some(rhs)) => {
      let s = lhs.val + rhs.val + carry;
      new_node(s % 10, solve(lhs.next, rhs.next, s / 10))
    },
  }
}

fn new_node(val: i32, next: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
  Some(Box::new(ListNode {
    val,
    next,
  }))
}
