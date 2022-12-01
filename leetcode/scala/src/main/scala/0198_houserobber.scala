// https://leetcode.com/problems/house-robber/
object Solution {
  def rob(nums: Array[Int]): Int = {
    var t = 0  // If we take
    var l1 = 0 // If we leave 1
    var l2 = 0 // If we leave 2

    for (n <- nums) {
      l2 = math.max(l1, l2)
      l1 = math.max(t, l2)
      t = l2 + n
    }

    math.max(math.max(l1, l2), t)
  }
}
