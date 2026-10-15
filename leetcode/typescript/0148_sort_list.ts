class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

function sortListPoorMemory(head: ListNode | null): ListNode | null {
  function toSortedArray(head: ListNode | null): number[] {
    let nums: number[] = [];
    while (head != null) {
      nums.push(head.val);
      head = head.next;
    }
    return nums.sort((a, b) => a - b);
  }

  function toList(nums: number[]): ListNode | null {
    let head: ListNode | null = null;
    let tail: ListNode | null = null;

    for (let n of nums) {
      if (head == null) {
        head = new ListNode(n);
        tail = head;
      } else {
        tail!.next = new ListNode(n);
        tail = tail!.next;
      }
    }
    return head;
  }

  return toList(toSortedArray(head));
}

function split(head: ListNode | null): [ListNode | null, ListNode | null] {
  let slow = head;
  let fast = head;

  let mid: ListNode | null = null;

  while (slow != null && !!fast?.next) {
    mid = slow;
    slow = slow.next;
    fast = fast.next.next;
  }

  const secondHalf = mid?.next || null;
  if (mid !== null) {
    mid.next = null;
  }
  return [head, secondHalf];
}

function merge(first: ListNode | null, second: ListNode | null): ListNode | null {
  if (first == null) return second;
  if (second == null) return first;

  if (first.val < second.val) {
    first.next = merge(first.next, second);
    return first;
  } else {
    second.next = merge(first, second.next);
    return second;
  }
}

function sortList(head: ListNode | null): ListNode | null {
  if (head === null || head.next === null) return head;

  const [lhs, rhs] = split(head);

  const sortedLhs = sortList(lhs);
  const sortedRhs = sortList(rhs);

  return merge(sortedLhs, sortedRhs);
}
