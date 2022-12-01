import scala.collection.mutable
// https://leetcode.com/problems/deepest-leaves-sum/
/*
 * Initial solution: DFS to find leaves, filter for only the deepest, then sum
 */
object DfsSolution {
  def deepestLeavesSum(root: TreeNode): Int = {
    if (root == null) return 0

    var maxDepth = 0
    val leaves = mutable.ArrayBuffer[(Int, TreeNode)]()
    val toVisit = mutable.Stack[(Int, TreeNode)]((0, root))

    while (toVisit.nonEmpty) {
      val (level, node) = toVisit.pop
      maxDepth = math.max(maxDepth, level)
      if (node.left == null && node.right == null) {
        leaves.addOne((level, node))
      } else {
        if (node.left != null) toVisit.push((level + 1, node.left))
        if (node.right != null) toVisit.push((level + 1, node.right))
      }
    }
    leaves
      .filter(_._1 == maxDepth)
      .map(_._2.value)
      .sum
  }
}

// FP solution: Find depth first, then sum only the values at that depth
object Solution {
  def deepestLeavesSum(root: TreeNode): Int = {
    sumAtDepth(root, findDepth(root))
  }

  // Sums when depth == 0
  def sumAtDepth(node: TreeNode, depth: Int): Int = {
    if (node == null) {
      0
    } else if (depth == 1) {
      node.value
    } else {
      sumAtDepth(node.left, depth - 1) + sumAtDepth(node.right, depth - 1)
    }
  }

  def findDepth(node: TreeNode): Int = {
    if (node == null) {
      0
    } else {
      1 + math.max(findDepth(node.left), findDepth(node.right))
    }
  }
}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
