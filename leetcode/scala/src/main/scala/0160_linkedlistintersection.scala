// https://leetcode.com/problems/intersection-of-two-linked-lists/

/* Approach: Negate the values in one of the given lists.
 * We can use fact that Node.val >= 1 to temporarily modify one of the lists.
 * Then, iterate over the other list. If we run into a negative value, it's the intersection.
 */
object Solution {
  def getIntersectionNode(headA: ListNode, headB: ListNode): ListNode = {
    negate(headA)
    var ptr = headB
    while (ptr != null && ptr.x > 0) {
      ptr = ptr.next
    }
    negate(headA)
    ptr
  }

  def negate(head: ListNode): Unit = {
    var ptr = head
    while (ptr != null) {
      ptr.x = -ptr.x
      ptr = ptr.next
    }
  }
}

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}
