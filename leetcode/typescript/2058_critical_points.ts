class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

function nodesBetweenCriticalPoints(head: ListNode | null): number[] {
  if (head == null) return [-1, -1];

  let prev = head;
  let cur = head?.next || null;

  let count = 0;
  let index = 1;

  let earliestPoint = -1;
  let latestPoint = -Infinity;
  let minDist = Infinity;

  while (!!cur?.next) {
    // Critical point
    if ((prev.val > cur.val && cur.next.val > cur.val) || (prev.val < cur.val && cur.next.val < cur.val)) {
      if (earliestPoint == -1) {
        earliestPoint = index;
      }
      minDist = Math.min(minDist, index - latestPoint);
      latestPoint = index;
      count++;
    }
    prev = cur;
    cur = cur.next;
    index++;
  }
  if (count < 2) return [-1, -1];

  return [minDist, latestPoint - earliestPoint];
}
