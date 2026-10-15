class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

function reverseList(head: ListNode | null): ListNode | null {
  let stack: number[] = [];
  for (let node = head; node != null; node = node.next) {
    stack.push(node.val);
  }
  for (let node = head; node != null; node = node.next) {
    node.val = stack.pop()!;
  }
  return head;
}
