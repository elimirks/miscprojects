// https://leetcode.com/problems/max-consecutive-ones/
object Solution {
  def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
    var maxLen = 0
    var curLen = 0

    var i = 0

    while (i < nums.length) {
      if (nums[i] == 0) {
        curLen = 0
      } else {
        curLen += 1

        if (curLen > maxLen) {
          maxLen = curLen
        }
      }

      i += 1
    }

    maxLen
  }
}
