import scala.collection.mutable

// https://leetcode.com/problems/can-make-arithmetic-progression-from-sequence/
// Naive solution: Sort first then check. But that's O(n log n)!
// Better solution is using a Set: will be O(n)
object Solution {
  def canMakeArithmeticProgression(arr: Array[Int]): Boolean = {
    val presentSet = arr.toSet

    presentSet.size <= 1 || (arr.size == presentSet.size &&
      isProgression(presentSet)
    )
  }

  def isProgression(values: Set[Int]): Boolean = {
    val (dist, min, max) = getBounds(values)

    values.forall(n => {
      if (n == min) {
        values.contains(n + dist)
      } else if (n == max) {
        values.contains(n - dist)
      } else {
        values.contains(n - dist) && values.contains(n + dist)
      }
    })
  }

  def getBounds(values: Set[Int]): (Int, Int, Int) = {
    var min       = Int.MaxValue
    var secondMin = Int.MaxValue
    var max       = Int.MinValue

    values.foreach(n => {
      if (n > max) {
        max = n
      }

      if (n < min) {
        secondMin = min
        min = n
      } else if (n < secondMin) {
        secondMin = n
      }
    })

    val dist = secondMin - min
    (dist, min, max)
  }
}
