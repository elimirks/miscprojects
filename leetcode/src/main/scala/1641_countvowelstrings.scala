// https://leetcode.com/problems/count-sorted-vowel-strings/

/*
 * Initial observation: the structure of this is triangular
 * What if, instead, we store the number of permutations for each letter for the previous iteration?
 * For example:
 * Iteration 1:
 * a -> 1
 * e -> 1
 * i -> 1
 * o -> 1
 * u -> 1
 * Iteration 2:
 * a -> 5
 * e -> 4
 * i -> 3
 * o -> 2
 * u -> 1
 * It will be easy to compute the next iteration number after that
 */
object Solution {
  def countVowelStrings(n: Int): Int = {
    var curA = 1
    var curE = 1
    var curI = 1
    var curO = 1
    var curU = 1
    for (_ <- 1 until n) {
      curO += curU
      curI += curO
      curE += curI
      curA += curE
    }
    curA + curE + curI + curO + curU
  }
}
