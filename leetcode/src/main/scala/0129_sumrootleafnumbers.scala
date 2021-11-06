import scala.collection.{mutable => mut}

// https://leetcode.com/problems/sum-root-to-leaf-numbers/
object Solution {
  def sumNumbers(root: TreeNode): Int = {
    var sum = 0
    val stack = mut.Stack[(TreeNode, Int)]((root, 0))

    while (stack.nonEmpty) {
      val (node, prevAcc) = stack.pop()
      val newAcc = prevAcc * 10 + node.value

      if (node.left == null && node.right == null) {
        // Leaf node
        sum += newAcc
      } else {
        if (node.left != null) stack.push((node.left, newAcc))
        if (node.right != null) stack.push((node.right, newAcc))
      }
    }

    sum
  }
}

// Purely functional, but will eventually stack overflow
object RecSolution {
  def sumNumbers(root: TreeNode): Int = {
    solve(root)
  }

  def solve(node: TreeNode, acc: Int = 0): Int = {
    val newAcc = acc * 10 + node.value

    if (node.left == null && node.right == null) {
      // Leaf node
      newAcc
    } else {
      (if (node.left != null) solve(node.left, newAcc) else 0) +
      (if (node.right != null) solve(node.right, newAcc) else 0)
    }
  }
}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
