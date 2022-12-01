// https://leetcode.com/problems/cousins-in-binary-tree/
object Solution {
  def isCousins(root: TreeNode, x: Int, y: Int): Boolean = {
    val (xParent, xDepth) = getParentAndDepth(root, x)

    if (xDepth == -1) {
      false
    } else {
      val (yParent, yDepth) = getParentAndDepth(root, y)

      if (yDepth == -1) {
        false
      } else {
        xDepth == yDepth && xParent != yParent
      }
    }
  }

  def getParentAndDepth(root: TreeNode, n: Int, depth: Int = 0): (TreeNode, Int) = {
    if (root == null) {
      (null, -1)
    } else if (root.left != null && root.left.value == n) {
      (root, depth + 1)
    } else if (root.right != null && root.right.value == n) {
      (root, depth + 1)
    } else {
      val (p, subDepth) = getParentAndDepth(root.left, n, depth + 1)

      if (p == null) {
        getParentAndDepth(root.right, n, depth + 1)
      } else {
        (p, subDepth)
      }
    }
  }
}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
