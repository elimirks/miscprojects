class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

function detectCycle(head: ListNode | null): ListNode | null {
  if (head == null) return null;

  let first: ListNode = head;
  let second: ListNode | null = head.next;

  while (first != second && first != second?.next) {
    first = first.next!; // Impossible to be null
    second = second?.next?.next || null;
    if (second == null) return null; // no cycles
  }

  // Store the nodes in the loop so we can detect the first entry
  // Not the most efficient, there may be a clever way to avoid O(n) memory
  let nodesInLoop: Set<ListNode> = new Set();
  while (!nodesInLoop.has(first)) {
    nodesInLoop.add(first);
    first = first.next!;
  }

  first = head;

  while (!nodesInLoop.has(first)) {
    first = first.next!;
  }
  return first
}
