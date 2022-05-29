// https://leetcode.com/problems/maximum-product-of-word-lengths/
import scala.collection.mutable

/*
 * Approach, looking for non-sharing, so we could store each word as a pair:
 * (letters: Set[Char], length: Int)
 * Question: With pairwise comparison, it can be O(n^2).
 *           Is there a way to do this without pairwise comp?
 * Another point: comparing with the largest first, descending, does
 * not guarantee we can short circuit the program. For example, there could be a
 * matching with a size 4 word and a size 1 word, but the best answer would
 * actually be a matching between two size 3 words.
 */
object CharMapSolution {
  def maxProduct(words: Array[String]): Int = {
    val charMap = createCharMap(words)
    words.map(word => findUncommonMax(word, charMap, words)).max
  }

  // With the given needle, return the answer to the question
  def findUncommonMax(
    needle: String,
    charMap: Map[Char, List[Int]],
    words: Array[String]
  ): Int = {
    val uncommon = mutable.Set[Int]()
    uncommon.addAll(0 until words.size)
    for {
      c           <- needle
      commonIndex <- charMap.getOrElse(c, List())
    } {
      uncommon.remove(commonIndex)
    }
    if (uncommon.size > 0) {
      needle.size * uncommon.map(i => words(i).size).max
    } else {
      0
    }
  }

  // TODO: Use an array instead of a map to be a bit faster
  // Creates a map of letter to word index
  def createCharMap(words: Array[String]): Map[Char, List[Int]] = {
    val charMap = mutable.Map[Char, List[Int]]()
    for {
      i <- 0 until words.size
      c <- words(i).toSet
    } {
      charMap.put(c, i :: charMap.getOrElse(c, List()))
    }
    charMap.toMap
  }
}

// Secondly: bitmask solution:
object Solution {
  def maxProduct(words: Array[String]): Int = {
    val bitmasks = words.map(wordToBitmask)
    var maxValue = 0
    for {
      i <- 0 until words.size
      j <- (i + 1) until words.size
      if ((bitmasks(i) & bitmasks(j)) == 0)
    } {
      maxValue = math.max(maxValue, words(i).size * words(j).size)
    }
    maxValue
  }

  def wordToBitmask(word: String): Int = {
    word.map(c => 1 << (c.toInt - 'a'.toInt)).foldLeft(0)(_ | _)
  }
}

println(Solution.maxProduct(Array("abcw","baz","foo","bar","xtfn","abcdef")))
