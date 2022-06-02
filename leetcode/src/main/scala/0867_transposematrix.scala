object Solution {
  def transpose(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    val height = matrix.size
    val width = matrix(0).size
    val result = Array.ofDim[Int](width, height)
    for {
      x <- 0 until width
      y <- 0 until height
    } {
      result(x)(y) = matrix(y)(x)
    }
    result
  }
}
