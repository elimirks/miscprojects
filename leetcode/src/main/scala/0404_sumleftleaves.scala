// https://leetcode.com/problems/sum-of-left-leaves/
import scala.collection.{mutable => mut}

// Faster solution, but far uglier
object Solution {
  def sumOfLeftLeaves(root: TreeNode): Int = {
    var left: TreeNode = null
    val rights = mut.Stack[TreeNode](root)

    var sum = 0

    // So abusive
    @inline
    def pushChildren(node: TreeNode): Unit = {
      left = node.left
      if (node.right != null) rights.push(node.right)
    }

    while (left != null || rights.nonEmpty) {
      if (left != null) {
        if (left.left == null && left.right == null) {
          sum += left.value
          left = null
        } else {
          pushChildren(left)
        }
      } else if (rights.nonEmpty) {
        val node = rights.pop()
        pushChildren(node)
      }
    }

    sum
  }
}

object RecSolution {
  def sumOfLeftLeaves(root: TreeNode): Int =
    if (root == null) 0 else sumOfLeft(root)

  @inline
  def sumOfLeft(node: TreeNode, isLeft: Boolean = false): Int = {
    if (node.left == null && node.right == null) {
      if (isLeft) node.value else 0
    } else {
      var sum = 0
      if (node.left != null)  sum += sumOfLeft(node.left, true)
      if (node.right != null) sum += sumOfLeft(node.right, false)
      sum
    }
  }
}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
