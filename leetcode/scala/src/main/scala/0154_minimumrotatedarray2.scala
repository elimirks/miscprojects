// https://leetcode.com/problems/find-minimum-in-rotated-sorted-array-ii/
import scala.collection.{mutable => m}

// Faster but uglier solution
// TL;DR try to do binary search. For edge cases, traverse both sides
object Solution {
  def findMin(nums: Array[Int]): Int = {
    // We don't _really_ care which data structure this is tbh
    // We'll eventually have to traverse the entire thing regardless
    // As long as it has O(1) adding & removing it's ok
    val toSearch = m.ArrayBuffer[(Int, Int)]((0, nums.size - 1))

    var minSoFar = Int.MaxValue

    while (toSearch.nonEmpty) {
      val (leftInd, rightInd) = toSearch.last
      toSearch.trimEnd(1)

      val size = rightInd - leftInd + 1

      if (size == 0) {
      } else if (size <= 2) {
        minSoFar = math.min(
          minSoFar,
          math.min(nums(leftInd), nums(rightInd))
        )
      } else {
        val leftVal = nums(leftInd)
        val rightVal = nums(rightInd)

        if (leftVal < rightVal) {
          minSoFar = math.min(minSoFar, leftVal)
        } else {
          minSoFar = math.min(minSoFar, rightVal)

          val midPoint = (rightInd + leftInd) / 2
          val midVal = nums(midPoint)

          if (leftVal >= midVal) {
            toSearch.append((leftInd + 1, midPoint - 1))
          } else { // leftVal < midVal
            minSoFar = math.min(minSoFar, leftVal)
          }

          if (midVal >= rightVal) {
            toSearch.append((midPoint + 1, rightInd - 1))
          } else { // midVal < rightVal
            minSoFar = math.min(minSoFar, midVal)
          }
        }
      }
    }

    minSoFar
  }
}
