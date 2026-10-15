class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

// Kinda cheating - use an intermediate array
function reorderList(head: ListNode | null): void {
  if (!head?.next?.next) return;

  let arrayForm: ListNode[] = [];
  while (head !== null) {
    arrayForm.push(head);
    let tmp = head.next;
    head.next = null;
    head = tmp;
  }

  for (let i = 0; i < arrayForm.length - 1; i++) {
    let rev = arrayForm.pop()!;
    if (i + 1 < arrayForm.length) {
      rev.next = arrayForm[i + 1];
    }
    arrayForm[i].next = rev;
  }
}
