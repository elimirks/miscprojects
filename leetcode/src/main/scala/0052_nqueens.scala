// Note: We could likely also use symmetry for this problem
object Solution {
  def totalNQueens(n: Int): Int = {
    subNQueens(n)
  }

  // usedLower and usedUpper are the lower (top left to bottom right) and upper diagonals
  def subNQueens(
    n: Int,
    row: Int = 0,
    usedCols: Set[Int] = Set(),
    usedUpper: Set[Int] = Set(),
    usedLower: Set[Int] = Set()
  ): Int = {
    if (row == n) {
      1
    } else {
      var combos = 0
      for (col <- 0 until n) {
        val (upper, lower) = diagIndices(n, col, row)
        if (!usedCols.contains(col) && !usedUpper.contains(upper) && !usedLower.contains(lower)) {
          combos += subNQueens(
            n,
            row + 1,
            usedCols + col,
            usedUpper + upper,
            usedLower + lower
          )
        }
      }
      combos
    }
  }

  /**
   * The associated diagonal index for the given coordinate
   * @return (upperDiagonal, lowerDiagonal)
   */ 
  def diagIndices(n: Int, x: Int, y: Int): (Int, Int) = {
    (n - y + x, y + x)
  }
}
