/**
 * Definition for singly-linked list.
 */
class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

function addTwoNumbers(l1: ListNode | null, l2: ListNode | null, carry: number = 0): ListNode | null {
  // Base case
  if (l1 == null && l2 == null && carry == 0) return null;

  let sum = (l1?.val || 0) + (l2?.val || 0) + carry;
  let nextCarry = Math.floor(sum / 10);
  let current = sum % 10;
  return new ListNode(current, addTwoNumbers(l1?.next || null, l2?.next || null, nextCarry));
}
