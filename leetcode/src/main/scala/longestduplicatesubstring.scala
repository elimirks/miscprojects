import scala.collection.{mutable => mut}
// https://leetcode.com/problems/longest-duplicate-substring/
object Solution {
  def longestDupSubstring(s: String): String = {
    var ans = ""
    var len = 1

    for (i <- 0 until s.size if i + len <= s.size) {
      var window = s.substring(i, i + len)
      val remaining = s.substring(i + 1)

      // Keep trying to grow the window until no more duplicates are found
      // `contains` runs in O(n) time
      while (remaining.contains(window)) {
        ans = window
        len += 1
        window = s.substring(i, i + len)
      }
    }

    ans
  }
}

// Simple but inefficient solution
object Simple {
  /*
   * Constraints:
   * - May overlap (e.g. for banana, ana is considered the longest)
   * This (bad) solution is O(2^n) since it must iterate every substring
   */
  def longestDupSubstring(s: String): String = {
    val subs = mut.Set[String]()

    for (len <- (1 until s.size).reverse) {
      for (i <- 0 until (s.size - len + 1)) {
        val sub = s.substring(i, i + len)

        if (subs.contains(sub)) {
          return sub
        }

        subs.add(sub)
      }

      subs.clear()
    }
    ""
  }
}
