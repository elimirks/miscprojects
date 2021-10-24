import scala.collection.mutable
// https://leetcode.com/problems/reverse-words-in-a-string/
object Solution {
  def reverseWords(s: String): String = {
    s.trim().split("\\s+").reverse.mkString(" ")
  }
}
