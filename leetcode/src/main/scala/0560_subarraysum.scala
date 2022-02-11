/**
  * https://leetcode.com/problems/subarray-sum-equals-k/
  * Given an array of integers nums and an integer k, return the
  * total number of continuous subarrays whose sum equals to k.
  */
object Solution {
  // O(n^2) time, O(n) space
  def subarraySum(nums: Array[Int], k: Int): Int = {
    val dyn = Array.ofDim[Int](nums.size + 1)
    dyn(0) = 0

    var acc = 0
    for (i <- (0 until nums.size)) {
      acc += nums(i)
      dyn(i + 1) = acc
    }

    var sum = 0
    for (i <- (0 until dyn.size)) {
      for (j <- (i + 1 until dyn.size)) {
        if (dyn(j) - dyn(i) == k) {
          sum += 1
        }
      }
    }
    sum
  }
}

println(Solution.subarraySum(Array(1, 1, 1), 2))
println(Solution.subarraySum(Array(1, 2, 3), 3))
println(Solution.subarraySum(Array(1), 1))
