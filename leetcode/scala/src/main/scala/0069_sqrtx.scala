// https://leetcode.com/problems/sqrtx/submissions/

object Solution {
  def mySqrt(x: Int): Int = {
    var res = 1
    while (x / res >= res) res += 1
    res - 1
  }
}
