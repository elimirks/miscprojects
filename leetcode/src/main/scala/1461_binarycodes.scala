// https://leetcode.com/problems/check-if-a-string-contains-all-binary-codes-of-size-k/

/*
 * First thought is permutations:
 * Find permutations of length k, then use a sliding window of size k to
 * remove each entry of the permutation set as you move along.
 * ... but that would require O(2^k) memory (way too much!)
 * ... and O(2^k) runtime, also way too much
 * Clearly, we cannot iterate over the permutations... what's the trick?
 */

/*
 * One solution for O(n) runtime and O(2^k) memory would be:
 * (plus time for hashing actually)
 * Slide the window, add all substrings to a set.
 * If the set is of size k!, we have a winner.
 * To reduce memory footprint, perhaps convert into integers
 */

import scala.collection.mutable

object Solution {
  def hasAllCodes(s: String, k: Int): Boolean = {
    val foundCodes = mutable.Set[String]()
    for (i <- 0 to s.length - k) {
      foundCodes.add(s.substring(i, i + k))
    }
    foundCodes.size == math.pow(2, k).toInt
  }
}
