import scala.collection.mutable
// https://leetcode.com/problems/populating-next-right-pointers-in-each-node-ii/
/*
 * Motivation: In-order traversal
 * Store a map of the current level right-most-visited node
 * If one already exists at the current level, create a next pointer, and then
 * update the right-most-node map
 */
object InOrderSolution {
  def connect(root: Node): Node = {
    val rightMost = mutable.HashMap[Int, Node]()
    def traverse(node: Node, level: Int): Unit = {
      if (node == null) {
        return
      }
      rightMost.get(level).foreach(previousNode => {
        previousNode.next = node
      })
      rightMost.put(level, node)
      traverse(node.left, level + 1)
      traverse(node.right, level + 1)
    }
    traverse(root, 0)
    root
  }
}

// BFS Solution. Trying to use less memory
object Solution {
  def connect(root: Node): Node = {
    var rightMost = root // Rightmost node of the current level
    var currentLevel = -1
    val toVisit = mutable.Queue[(Int, Node)](
      (0, root)
    )
    while (toVisit.nonEmpty) {
      val (level, node) = toVisit.dequeue
      if (node != null) {
        if (currentLevel != level) {
          rightMost = node
          currentLevel = level
        } else {
          rightMost.next = node
          rightMost = node
        }
        toVisit.enqueue((level + 1, node.left))
        toVisit.enqueue((level + 1, node.right))
      }
    }
    root
  }
}

class Node(_value: Int, _left: Node = null, _right: Node = null) {
  var value: Int  = _value
  var left: Node  = _left
  var right: Node = _right
  var next: Node  = null

  def prettyPrint(level: Int = 0): Unit = {
    val indent = (0 until level).map(_ => ' ').mkString
    val nextValue = if (next == null) "null" else next.value.toString
    println(s"${indent}${value} -> ${nextValue}")

    if (left != null)  left.prettyPrint(level + 1)
    if (right != null) right.prettyPrint(level + 1)
  }
}

val tree = new Node(1,
  new Node(2,
    new Node(4),
    new Node(5),
  ),
  new Node(3,
    null,
    new Node(7),
  ),
)

tree.prettyPrint()

Solution.connect(tree).prettyPrint()
