// Input: a = "abcd", b = "cdabcdab"
// Output: 3
// Explanation: We return 3 because by repeating a three times "abcdabcdabcd", b is a substring of it.

// https://leetcode.com/problems/repeated-string-match/
object Solution {
  def cmp(rep: String, sub: String, index: Int): Int = {
    var i = 0
    var j = index

    var times = 1

    while (i < sub.length) {
      if (j >= rep.length) {
        times += 1
        j = 0
      }

      if (rep.charAt(j) != sub.charAt(i)) {
        return -1
      }

      i += 1
      j += 1
    }

    times
  }

  def repeatedStringMatch(rep: String, sub: String): Int = {
    var i = 0

    while (i < rep.length) {
      val res = cmp(rep, sub, i)

      if (res >= 0) {
        return res
      }

      i += 1
    }
    -1
  }
}
