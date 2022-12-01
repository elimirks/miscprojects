/**
  * Given the binary representation of an integer as a string s, return the
  * number of steps to reduce it to 1 under the following rules:
  *
  * 1. If the current number is even, you have to divide it by 2.
  * 2. If the current number is odd, you have to add 1 to it.
  *
  * It is guaranteed that you can always reach one for all test cases.
  *
  * https://leetcode.com/problems/number-of-steps-to-reduce-a-number-in-binary-representation-to-one/
  */
object Solution {
  def numSteps(s: String): Int = {
    if (s == "1") {
      0
    } else {
      if (s.last == '0') {
        numSteps(div2(s)) + 1
      } else {
        numSteps(increment(s)) + 1
      }
    }
  }

  def div2(s: String): String =
    s.substring(0, s.length - 1)

  def increment(s: String): String = {
    val rev = s.reverse
    val firstZero = rev.indexOf('0')

    if (firstZero == -1) {
      "1" + (0 until s.length).map(_ => '0').mkString
    } else {
      s.substring(0, s.length - firstZero - 1) +
        "1" +
        (0 until firstZero).map(_ => '0').mkString
    }
  }
}
