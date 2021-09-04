// Input: haystack = "hello", needle = "ll"
// Output: 2

object Solution {
  // Assumes superStr.length - index >= subStr.length
  def cmp(superStr: String, index: Int, subStr: String): Boolean = {
    var i = 0

    while (i < subStr.length) {
      if (superStr.charAt(i + index) != subStr.charAt(i)) {
        return false
      }

      i += 1
    }

    true
  }

  def strStr(haystack: String, needle: String): Int = {
    if (haystack.length == 0 && needle.length == 0) {
      return 0
    }

    var i = 0

    while (i < haystack.length - needle.length + 1) {
      if (cmp(haystack, i, needle)) {
        return i
      }

      i += 1
    }

    -1
  }
}
