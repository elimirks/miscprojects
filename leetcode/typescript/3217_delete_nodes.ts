class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

function modifiedList(nums: number[], head: ListNode | null): ListNode | null {
  let numSet = new Set(nums);

  // Step 1: Remove the leading nodes that are in the set
  while (head !== null && numSet.has(head.val)) {
    head = head.next;
  }
  let iterTail = head;

  // Step 2: Iterate over remaining nodes
  while (!!iterTail?.next) {
    if (numSet.has(iterTail.next.val)) {
      iterTail.next = iterTail.next.next;
    } else {
      iterTail = iterTail.next;
    }
  }
  return head;
}
