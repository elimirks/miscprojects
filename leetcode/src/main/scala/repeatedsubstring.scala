// Input: s = "abab"
// Output: true
// Explanation: It is the substring "ab" twice.

object Solution {
  // Assumes str.length - index >= subLen
  def cmp(str: String, index: Int, subLen: Int): Boolean = {
    var i = 0

    while (i < subLen) {
      if (str.charAt(i + index) != str.charAt(i)) {
        return false
      }

      i += 1
    }

    true
  }

  // O(n)
  def attempt(str: String, size: Int): Boolean = {
    var i = 0

    while (i < str.length) {
      if (!cmp(str, i, size)) {
        return false
      }

      i += size
    }

    true
  }

  // O(n^2)
  def repeatedSubstringPattern(str: String): Boolean = {
    var size = str.length / 2

    // Iterates about O(n / 2) times, so on the order of O(n)
    while (size > 0) {
      if (str.length % size == 0 && attempt(str, size)) {
        return true
      }

      size -= 1
    }

    false
  }
}
