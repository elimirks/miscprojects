// https://leetcode.com/problems/sum-of-absolute-differences-in-a-sorted-array/
// result[i] is equal to sum(|nums[i]-nums[j]|) where 0 <= j < nums.length
// NOTE: The leetcode evaluator for this problem is broken
object Solution {
  def getSumAbsoluteDifferences(nums: Array[Int]): Array[Int] = {
    val res = Array.ofDim[Int](nums.size)

    for (i <- 0 until nums.length) {
      var acc = 0

      for (nj <- nums) {
        acc += (nums(i) - nj).abs
      }

      res(i) = acc
    }

    res
  }
}
