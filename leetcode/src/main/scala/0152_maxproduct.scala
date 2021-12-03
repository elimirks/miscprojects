// https://leetcode.com/problems/maximum-product-subarray/
object Solution {
  // O(n) time, O(1) memory, but super ugly
  def maxProduct(nums: Array[Int]): Int = {
    var max = nums.max // Handle the case where everything is negative
    var start = 0
    var finish = nums.indexOf(0)
    // Run the algorithm between zeros
    while (finish != -1) {
      max = math.max(max, subMax(nums, start, finish))
      start = finish + 1
      finish = nums.indexOf(0, start)
    }
    math.max(max, subMax(nums, start, nums.length))
  }

  // Runs the algorithm on a subarray with no 0s in it
  def subMax(nums: Array[Int], start: Int, finish: Int): Int = {
    // dp the sub array (between 0s)
    for (i <- (start + 1) until finish) {
      nums(i) = nums(i) * nums(i - 1)
    }

    if (finish - start <= 1) {
      // Handle cases where the range is 0 or 1 element
      nums(0)
    } else if (nums(finish - 1) > 0) {
      nums(finish - 1)
    } else {
      var lastNeg = finish - 1
      while (lastNeg > start && nums(lastNeg) < 0) {
        lastNeg -= 1
      }

      var firstNeg = start
      while (nums(firstNeg) > 0 && firstNeg <= lastNeg) {
        firstNeg += 1
      }

      math.max(nums(finish - 1) / nums(firstNeg), nums(lastNeg))
    }
  }
}

println(Solution.maxProduct(Array(2, 3, -2, 4)))
println(Solution.maxProduct(Array(-2, 0, -1)))
println(Solution.maxProduct(Array(-2)))
println(Solution.maxProduct(Array(0, 0, -3, 1)))
