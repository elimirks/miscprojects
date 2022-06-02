// https://leetcode.com/problems/coin-change/
import scala.collection.mutable

/*
 * Greedy can't really work. What about the case [1, 4, 5], 8? Picking 5 would be suboptimal
 * Or for the case [4, 5], 8, picking 5 would actually fail the problem!
 * ok so.. what are the facts? The brute force technique is trivial but ofc won't work well.
 * Greedy will fail on edge cases.
 * Is DP an option here? For different amounts of money leading up to `amount`, cache the best cases?
 */

// :(
object PoorMemorySolution {
  def coinChange(coins: Array[Int], amount: Int): Int = {
    // -1 will be placed for values that are impossible
    val dyn = mutable.HashMap[Int, Int]()
    dyn.put(0, 0)
    for (coin <- coins) {
      dyn.put(coin, 1) // Only takes 1 coin for each of these cases right?
    }
    // Handle special cases
    dyn.get(amount) match {
      case Some(res) => return res
      case _ => {}
    }

    // DP + BFS
    val toSearch = mutable.Queue[Int](amount)
    while (toSearch.nonEmpty) {
      val nextAmount = toSearch.dequeue
      // Check if all the "children" have been computed
      var childNeedsComputing = false
      for (coin <- coins if nextAmount - coin >= 0) {
        val lowerAmount = nextAmount - coin
        if (!dyn.contains(lowerAmount)) {
          toSearch.enqueue(lowerAmount)
          childNeedsComputing = true
        }
      }
      // If not, re-queue the current value for now
      if (childNeedsComputing) {
        toSearch.enqueue(nextAmount)
      } else {
        val counts = coins
          .map(nextAmount - _)
          .flatMap(dyn.get(_))
          .filter(_ >= 0)

        if (counts.size > 0) {
          val min = counts.foldLeft(Int.MaxValue)(math.min)
          dyn.put(nextAmount, min + 1)
        } else {
          dyn.put(nextAmount, -1)
        }
      }
    }
    dyn.getOrElse(amount, -1)
  }
}

import scala.collection.mutable
object Solution {
  def coinChange(coins: Array[Int], amount: Int): Int = {
    val dyn = Array.ofDim[Int](amount + 1)
    dyn(0) = 0
    for (n <- 1 to amount) {
      var minLower = Int.MaxValue
      for (coin <- coins if n - coin >= 0) {
        val lowerAmount = dyn(n - coin)
        if (lowerAmount != Int.MaxValue) {
          minLower = math.min(minLower, lowerAmount + 1)
        }
      }
      dyn(n) = minLower
    }
    if (dyn(amount) == Int.MaxValue) -1 else dyn(amount)
  }
}

Solution.coinChange(Array(1, 2, 5), 11)
