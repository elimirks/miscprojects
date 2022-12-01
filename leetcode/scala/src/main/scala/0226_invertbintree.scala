// https://leetcode.com/problems/invert-binary-tree/
object Solution {
  def invertTree(root: TreeNode): TreeNode = {
    if (root == null) {
      null
    } else {
      new TreeNode(
        root.value,
        invertTree(root.right),
        invertTree(root.left)
      )
    }
  }
}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
