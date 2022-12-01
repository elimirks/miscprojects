// https://leetcode.com/problems/two-sum-ii-input-array-is-sorted/
/**
  * Approach: Iterate over numbers. Use binary search to find second number.
  * Runtime will be O(n log n), space will be O(1)
  */
object Solution {
  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    for (i <- 1 until numbers.size) {
      val firstNum = numbers(i)
      val secondIndex = bin(numbers, 0, i - 1, target - firstNum)

      if (secondIndex != -1) {
        // Add 1 because... the question indexes from 1 for some reason
        return Array(1 + secondIndex, 1 + i)
      }
    }
    ??? // Should never happen, since we're guaranteed a valid input
  }

  // Returns the index of the second num, or -1 if not found
  @scala.annotation.tailrec
  def bin(numbers: Array[Int], start: Int, finish: Int, target: Int): Int = {
    if (start > finish) {
      -1
    } else {
      val mid = (finish + start) / 2
      val midNum = numbers(mid)

      if (midNum > target) {
        bin(numbers, start, mid - 1, target)
      } else if (midNum < target) {
        bin(numbers, mid + 1, finish, target)
      } else {
        mid
      }
    }
  }
}
