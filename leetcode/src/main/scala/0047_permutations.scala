/*
 * Thought 1: Brute force via recursion
 * Thought 2: Permutation tree?
 */
object Solution {
  def permuteUnique(nums: Array[Int]): List[List[Int]] = {
    solve(nums, 0).toList
  }

  def solve(nums: Array[Int], start: Int = 0): Set[List[Int]] = {
    nums.size - start match {
      case 0 => Set()
      case 1 => Set(List(nums.last))
      case _ =>
        val n = nums(start)
        val recLists = solve(nums, start + 1)
        // And now, scatter `n` throughout each position of `rec` and call `distinct`
        for {
          recList <- recLists
          pivot   <- 0 to (nums.size - start - 1)
        } yield {
          // O(n)
          val (lhs, rhs) = recList.splitAt(pivot)
          lhs ++ (n +: rhs)
        }
    }
  }
}

println(Solution.permuteUnique(Array(
  1, 2
)))

println(Solution.permuteUnique(Array(
  1, 1, 2
)))

println(Solution.permuteUnique(Array(
  1, 2, 3
)))
