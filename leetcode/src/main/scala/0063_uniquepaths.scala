import scala.collection.mutable
// https://leetcode.com/problems/unique-paths-ii/

/* Approach: BFS out from the bottom right of the grid.
 * ... but this runs out of memory on the leetcode site
 */
object BfsSolution {
  // obstactleGrid is a list of rows
  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {
    val height = obstacleGrid.size
    val width = obstacleGrid(0).size

    if (obstacleGrid(height - 1)(width - 1) == 1) return 0
    if (obstacleGrid(0)(0) == 1) return 0

    // First off, set obstacles to be impossible paths
    for {
      i <- 0 until height
      j <- 0 until width
    } {
      if (obstacleGrid(i)(j) == 1) {
        obstacleGrid(i)(j) = -1
      }
    }

    obstacleGrid(height - 1)(width - 1) = 1
    val toVisit = mutable.Queue[(Int, Int)]()

    if (width > 1) toVisit.enqueue((width - 2, height - 1))
    if (height > 1) toVisit.enqueue((width - 1, height - 2))

    while (toVisit.nonEmpty) {
      val (x, y) = toVisit.dequeue

      if (obstacleGrid(y)(x) != -1) {
        val rightWays = if (x == width - 1 || obstacleGrid(y)(x + 1) == -1) {
          0
        } else {
          obstacleGrid(y)(x + 1)
        }
        val downWays = if (y == height - 1 || obstacleGrid(y + 1)(x) == -1) {
          0
        } else {
          obstacleGrid(y + 1)(x)
        }
        obstacleGrid(y)(x) = rightWays + downWays
      }

      if (x > 0) toVisit.enqueue((x - 1, y))
      if (y > 0) toVisit.enqueue((x, y - 1))
    }
    obstacleGrid(0)(0)
  }
}

object Solution {
  // obstactleGrid is a list of rows
  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {
    val height = obstacleGrid.size
    val width = obstacleGrid(0).size

    // Dyn the first row
    for (x <- 0 until width) {
      if (obstacleGrid(0)(x) == 1) {
        obstacleGrid(0)(x) = 0
      } else if (x == 0) {
        obstacleGrid(0)(x) = 1
      } else {
        obstacleGrid(0)(x) = obstacleGrid(0)(x - 1)
      }
    }
    // Dyn the first column
    for (y <- 1 until height) {
      if (obstacleGrid(y)(0) == 1) {
        obstacleGrid(y)(0) = 0
      } else {
        obstacleGrid(y)(0) = obstacleGrid(y - 1)(0)
      }
    }
    // Dyn the rest
    for {
      y <- 1 until height
      x <- 1 until width
    } {
      if (obstacleGrid(y)(x) == 1) {
        obstacleGrid(y)(x) = 0
      } else {
        obstacleGrid(y)(x) = obstacleGrid(y - 1)(x) + obstacleGrid(y)(x - 1)
      }
    }
    obstacleGrid(height - 1)(width - 1)
  }
}
