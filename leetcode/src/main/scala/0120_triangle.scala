// https://leetcode.com/problems/triangle
/*
 * Motivation:
 * We start off with a single array of size n, then for each row (bottom up), we:
 * 1) add the next row to the array
 * 2) reduce each choice of two using min
 *
 * For the example test case:
 *        4  1 8  3
 * reduce 1  1 3
 *      + 6  5 7 
 *      = 7  6 10
 * reduce 6  6
 *    add 3  4
 *      = 9  10
 * reduce 9
 *    add 2
 *      = 11
 */
object Solution {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    val size = triangle.size
    val dyn = Array.ofDim[Int](size)

    for (i <- (0 until size).reverse) {
      val row = triangle(i)
      // Add the row to the current result
      for (j <- 0 until row.size) {
        dyn(j) += row(j)
      }
      // Reduce down to the next higher row length
      for (j <- 0 until row.size - 1) {
        dyn(j) = math.min(dyn(j), dyn(j + 1))
      }
    }
    dyn(0)
  }
}
