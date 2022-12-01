// https://leetcode.com/problems/running-sum-of-1d-array/
object Solution {
  def runningSum(nums: Array[Int]): Array[Int] = {
    for (i <- 1 until nums.size) {
      nums(i) = nums(i - 1) + nums(i)
    }
    nums
  }
}
