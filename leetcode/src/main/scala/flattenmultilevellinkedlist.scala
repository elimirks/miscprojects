// https://leetcode.com/problems/flatten-a-multilevel-doubly-linked-list/
object Solution {
  def flatten(head: Node): Node = {
    var node = head

    while (node != null) {
      flattenChild(node)
      node = node.next
    }

    head
  }

  def flattenChild(node: Node): Unit = {
    if (node.child != null) {
      val subHead = node.child
      val next = node.next

      node.child = null
      node.next = subHead
      subHead.prev = node

      if (next != null) {
        val subTail = getTail(subHead)
        subTail.next = next
        next.prev = subTail
      }
    }
  }

  def getTail(node: Node): Node =
    if (node.next == null) node else getTail(node.next)
}

class Node(var _value: Int) {
  var value: Int = _value
  var prev: Node = null
  var next: Node = null
  var child: Node = null
}
