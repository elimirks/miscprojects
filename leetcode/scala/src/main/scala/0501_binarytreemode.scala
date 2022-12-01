import scala.collection.{mutable => m}

/**
  * https://leetcode.com/problems/find-mode-in-binary-search-tree/
  * Definition for a binary tree node.
  * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  *   var value: Int = _value
  *   var left: TreeNode = _left
  *   var right: TreeNode = _right
  * }
  */
object Solution {
  def findMode(root: TreeNode): Array[Int] = {
    val frequencies = getFrequencies(root)
    val maxFrequency = frequencies.values.max

    val modes = m.ArrayBuffer[Int]()

    frequencies.foreach({ case (value, frequency) =>
      if (frequency == maxFrequency) {
        modes.addOne(value)
      }
    })

    modes.toArray
  }

  def getFrequencies(root: TreeNode): m.HashMap[Int, Int] = {
    val frequencies = m.HashMap[Int, Int]()

    val stack = m.Stack[TreeNode](root)
    while (stack.nonEmpty) {
      val node = stack.pop()

      val newFrequency = frequencies.getOrElse(node.value, 0) + 1
      frequencies.put(node.value, newFrequency)

      if (node.left != null)  stack.push(node.left)
      if (node.right != null) stack.push(node.right)
    }

    frequencies
  }
}

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
