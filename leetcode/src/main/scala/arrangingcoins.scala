// https://leetcode.com/problems/arranging-coins/
object Solution {
  def arrangeCoins(n: Int): Int =
    /*
     * Math solution, O(1)
     * The inverse of `arrangeCoins` is:
     * inv(n) = (1 until n).sum = n(n + 1) / 2
     * Solving for the inverse of the inverse, we get the quadratic:
     * f^2 + f - 2n = 0
     * Solve for f and floor since we want to truncate the bottom row
     */
    ((math.sqrt(1.0 + 8.0 * n) - 1.0) / 2.0).toInt
}

object IterSolution {
  def arrangeCoins(n: Int): Int = {
    var i = 0L
    var count = 0L

    while (i + count < n) {
      count += 1
      i += count
    }

    count.toInt
  }
}
