import scala.collection.{mutable => mut}
// https://leetcode.com/problems/single-number-iii/
object Solution {
  // Simple solution, but it's O(n) runtime and O(n) memory usage
  def singleNumber(nums: Array[Int]): Array[Int] = {
    // The two unique numbers xored together
    val xored = nums.foldLeft(0)(_ ^ _)

    /*
     * The rightmost set bit will only be in _one_ of the two numbers.
     * So by keeping two separate trackers based on if the
     * rightmost bit is set, we split the problem in half:
     * Into the regular xor trick for a single unique number
     *
     * Note: It actually doesn't matter which set bit we choose from xored value.
     *       We could pick the leftmost set bit and it would also work.
     *       Or the middle-most set bit, or a completely random set bit!
     */
    val pickedBit = xored & ~(xored - 1)

    var a = 0
    var b = 0

    for (n <- nums) {
      if ((n & pickedBit) == 0) {
        a ^= n
      } else {
        b ^= n
      }
    }

    Array(a, b)
  }
}

object SetSolution {
  // Simple solution, but it's O(n) runtime and O(n) memory usage
  def singleNumber(nums: Array[Int]): Array[Int] = {
    val xs = mut.Set[Int]()

    nums.foreach(n => {
      if (xs.contains(n)) {
        xs.remove(n)
      } else {
        xs.add(n)
      }
    })

    xs.toArray
  }
}
