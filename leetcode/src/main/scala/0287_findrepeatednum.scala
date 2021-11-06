// https://leetcode.com/problems/find-the-duplicate-number/
object Solution {
  // Rubbish runtime
  def findDuplicate(nums: Array[Int]): Int = {
    for (i <- 0 until nums.length) {
      val n = nums(i)

      for (j <- i + 1 until nums.length) {
        if (n == nums(j)) {
          return n
        }
      }
    }

    0
  }
}
