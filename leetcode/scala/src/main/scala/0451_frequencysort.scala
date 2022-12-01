import scala.collection.mutable
/*
 * https://leetcode.com/problems/sort-characters-by-frequency/
 * Given a string s, sort it in decreasing order based on the frequency of the characters.
 * The frequency of a character is the number of times it appears in the string.
 * @example
 * Input: s = "tree"
 * Output: "eert"
 */
object Solution {
  def frequencySort(s: String): String = {
    val counts = mutable.Map[Char, Int]()

    s.toSeq.foreach(c => {
      val newCount = counts.getOrElse(c, 0) + 1
      counts.put(c, newCount)
    })

    val retString = new mutable.StringBuilder()

    for {
      (c, count) <- counts.toSeq.sortBy(_._2).reverse
      _          <- 0 until count
    } retString.append(c)

    retString.toString()
  }
}
