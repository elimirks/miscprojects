// https://leetcode.com/problems/best-time-to-buy-and-sell-stock-ii/submissions/
object Solution {
  def maxProfit(prices: Array[Int]): Int = {
    (1 until prices.size)
      .map(i => math.max(0, prices(i) - prices(i - 1)))
      .sum
  }
}
