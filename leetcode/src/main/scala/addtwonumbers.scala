// https://leetcode.com/problems/add-two-numbers/
/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
object Solution {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    addEqual(l1, l2)
  }

  def addEqual(l1: ListNode, l2: ListNode, carry: Int = 0): ListNode = {
    var sum = carry
    var ret: ListNode = null
    var l1Next: ListNode = null
    var l2Next: ListNode = null

    if (l1 != null) {
      sum += l1.x
      l1Next = l1.next
      ret = l1 // Reuse the memory
    }

    if (l2 != null) {
      sum += l2.x
      l2Next = l2.next
      ret = l2
    }

    if (ret != null) {
      ret.x = sum % 10
      ret.next = addEqual(l1Next, l2Next, sum / 10)
      ret
    } else if (carry == 0) {
      null
    } else {
      new ListNode(carry)
    }
  }
}

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x

  override def toString(): String = {
    if (next == null)
      x.toString
    else
      x.toString + " -> " + next.toString()
  }
}

object ListNode {
  def of(xs: Int*): ListNode = {
    xs.foldRight(null: ListNode)((it, acc) => {
      new ListNode(it, acc)
    })
  }
}

println(
  Solution.addTwoNumbers(
    ListNode.of(2, 4, 3),
    ListNode.of(5, 6, 4)
  ).toString
)

println(
  Solution.addTwoNumbers(
    ListNode.of(0),
    ListNode.of(0)
  ).toString
)

println(
  Solution.addTwoNumbers(
    ListNode.of(9, 9, 9, 9, 9, 9, 9),
    ListNode.of(9, 9, 9, 9)
  ).toString
)

println(
  Solution.addTwoNumbers(
    ListNode.of(9, 9),
    ListNode.of(9)
  ).toString
)
