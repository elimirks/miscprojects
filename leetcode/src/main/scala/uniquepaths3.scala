import scala.collection.{mutable => mut}

// https://leetcode.com/problems/unique-paths-iii/submissions/
object Solution {
  def uniquePathsIII(grid: Array[Array[Int]]): Int = {
    val board = new Board(grid)
    val emptyCount = board.countEmpties()
    val (startX, startY) = board.findStarting()

    /* Stores (x, y, visited)
     * Use immutable HashSet because it's very memory efficient for the
     * add+copy operation. And it's an O(1) amortized runtime.
     * It will effectively let us "rewind" each possible path
     */
    val toSearch = mut.Stack[(Int, Int, HashSet[(Int, Int)])]()

    @inline
    def tryPush(x: Int, y: Int, visited: Set[(Int, Int)]): Unit = {
      if (
        x >= 0 && y >= 0 && x < board.width && y < board.height &&
          board.grid(y)(x) != -1 &&
          !visited.contains((x, y))
      ) {
        toSearch.push((x, y, visited))
      }
    }

    var numPaths = 0

    toSearch.push((startX, startY, Set((startX, startY))))

    /* Augmented DFS to traverse every path, no early termination
     * So it's worse than O(n).
     * With DFS, `toSearch` will be at most of length `emptyCount * 3`
     * ^ since the traversal has to "pick and pop" one of the added nodes
     * With BFS, `toSearch` would get larger, since it eventually has to store
     * every single possible path in memory at once in the worst case.
     */
    while (toSearch.nonEmpty) {
      val (x, y, visited) = toSearch.pop()

      if (visited.size - 1 == emptyCount && board.grid(y)(x) == 2) {
        numPaths += 1
      } else {
        val newVisited = visited + ((x, y))
        tryPush(x + 1, y, newVisited)
        tryPush(x - 1, y, newVisited)
        tryPush(x, y + 1, newVisited)
        tryPush(x, y - 1, newVisited)
      }
    }

    numPaths
  }
}

class Board(_grid: Array[Array[Int]]) {
  val grid = _grid

  val height = grid.size
  val width = grid(0).size

  def findStarting(): (Int, Int) = {
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      if (grid(y)(x) == 1) {
        return (x, y)
      }
    }

    (-1, -1)
  }

  def countEmpties(): Long = {
    var acc = 0L

    for {
      y <- 0 until height
      x <- 0 until width
    } {
      if (grid(y)(x) == 0) {
        acc += 1L
      }
    }

    acc
  }
}
