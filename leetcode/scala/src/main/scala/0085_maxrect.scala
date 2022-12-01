// https://leetcode.com/problems/maximal-rectangle
import scala.collection.{mutable => mut}
object Solution {
  def maximalRectangle(matrix: Array[Array[Char]]): Int = {
    if (matrix.length == 0) return 0

    val rows = matrix.length
    val cols = matrix(0).length

    var max = 0
    // Tracks the keys (hsizes) we inserted into `scanned`
    // Should always be stored in INCREASING order of hsize
    val hsizes  = mut.ArrayBuffer[Int](0)
    val stretches = mut.ArrayBuffer[Int](0)
    val column = mut.ArrayBuffer.fill[Int](rows)(0)

    for (x <- 0 until cols) {
      for (y <- 0 until rows) {
        if (matrix(y)(x) == '1') {
          column(y) += 1
        } else {
          column(y) = 0
        }

        val lastSize = hsizes.last
        val hsize = column(y)

        if (hsize == lastSize) {
          stretches(stretches.length - 1) += 1
        } else if (hsize > lastSize) {
          hsizes.append(hsize)
          stretches.append(1)
        } else if (hsize < lastSize) {
          var len = 0

          while (hsize < hsizes.last) {
            len += stretches.last
            max = math.max(max, hsizes.last * len)
            hsizes.trimEnd(1)
            stretches.trimEnd(1)
          }
          len += 1

          if (hsizes.last == hsize) {
            stretches(stretches.length - 1) += len
          } else {
            hsizes.append(hsize)
            stretches.append(len)
          }
        }
      }

      while (hsizes.length > 1) {
        val len = stretches.last
        max = math.max(max, hsizes.last * len)

        hsizes.trimEnd(1)
        stretches.trimEnd(1)

        stretches(stretches.length - 1) += len
      }
    }

    max
  }
}

// println(Solution.maximalRectangle(Array(
//   Array('1','1','1','1','1','1','1','1'),
//   Array('1','1','1','1','1','1','1','0'),
//   Array('1','1','1','1','1','1','1','0'),
//   Array('1','1','1','1','1','0','0','0'),
//   Array('0','1','1','1','1','0','0','0')
// )))

println(Solution.maximalRectangle(Array(
  Array('0','0','1','0'),
  Array('1','1','1','1'),
  Array('1','1','1','1'),
  Array('1','1','1','0'),
  Array('1','1','0','0'),
  Array('1','1','1','1'),
  Array('1','1','1','0'),
)))
