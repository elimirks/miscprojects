class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

function listLen(head: ListNode | null): number {
  if (head == null) return 0;
  return 1 + listLen(head.next);
}

function removeNode(head: ListNode | null, n: number): ListNode | null {
  if (head == null) return null;
  if (n == 0) return head.next;
  return new ListNode(head.val, removeNode(head.next, n - 1));
}

function removeNthFromEnd(head: ListNode | null, n: number): ListNode | null {
  const len = listLen(head);
  if (len <= 1) return null;

  // Index to remove
  const np = len - n;
  return removeNode(head, np);
}
