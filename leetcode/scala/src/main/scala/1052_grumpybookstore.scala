// https://leetcode.com/problems/grumpy-bookstore-owner/
object Solution {
  def maxSatisfied(
      customers: Array[Int],
      grumpy: Array[Int],
      minutes: Int
  ): Int = {
    // O(n)
    val baseCount = customers
      .zip(grumpy)
      .map({ case (count, grump) =>
        if (grump == 0) count else 0
      })
      .sum

    val ungrumpyCounts = customers
      .zip(grumpy)
      .map({ case (count, grump) =>
        if (grump == 1) count else 0
      })

    // O(n)
    val (_, left) = ungrumpyCounts.foldLeft((0, List(0)))(dpFolder)
    val leftDP = left.reverse.toArray

    // O(n)
    (0 to customers.length - minutes).foldLeft(baseCount)((acc, i) => {
      acc.max(baseCount + leftDP(i + minutes) - leftDP(i))
    })
  }

  def dpFolder(acc: (Int, List[Int]), it: Int): (Int, List[Int]) = {
    val (sum, previous) = acc
    val newSum = sum + it
    (newSum, newSum :: previous)
  }
}
