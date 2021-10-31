// https://leetcode.com/problems/flatten-binary-tree-to-linked-list/
object Solution {
  def flatten(root: TreeNode): Unit = {
    var node = root

    while (node != null) {
      flattenChild(node)
      node = node.right
    }
  }

  def flattenChild(node: TreeNode): Unit = {
    if (node.left != null) {
      val left = node.left
      val right = node.right

      node.left = null
      node.right = left

      if (right != null) {
        val subTail = getRightTail(left)
        subTail.right = right
      }
    }
  }

  // Should just be @tailrec'd, but apparently leetcode doesn't like @tailrec
  def getRightTail(node: TreeNode): TreeNode = {
    var n = node
    while (n.right != null) n = n.right
    n
  }
}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
