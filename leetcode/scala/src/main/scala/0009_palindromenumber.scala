// https://leetcode.com/problems/palindrome-number/
object LazySolution {
  def isPalindrome(x: Int): Boolean = {
    val s = x.toString
    s == s.reverse
  }
}

object Solution {
  def isPalindrome(x: Int): Boolean = {
    if (x < 0) return false
    val maxDigit = math.log10(x).floor.toInt
    var upperMultiplier = math.pow(10, maxDigit).toInt
    var lowerMultiplier = 1

    while (lowerMultiplier < upperMultiplier) {
      val lower = (x / lowerMultiplier) % 10
      val upper = (x / upperMultiplier) % 10
      if (lower != upper) return false
      lowerMultiplier *= 10
      upperMultiplier /= 10
    }
    true
  }
}
