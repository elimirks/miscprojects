import scala.collection.mutable

// https://leetcode.com/problems/generate-parentheses/submissions/
object Solution {
  def generateParenthesis(n: Int): List[String] = {
    // Rule 1: Place inside: s"($s)"
    // Rule 2: Combine all previous paren solutions that sum to n, lhs & rhs
    val solutions = mutable.Map[Int, mutable.Set[String]](
      0 -> mutable.Set(),
      1 -> mutable.Set("()")
    )

    for (i <- 2 to n) {
      val iSolutions = mutable.Set[String]()

      // Rule 1:
      solutions.get(i - 1).get.foreach(s => {
        iSolutions.add(s"($s)")
      })

      // Rule 2:
      for (j <- 1 until (i / 2) + 1) {
        val k = i - j
        val pjs = solutions.get(j).get
        val pks = solutions.get(k).get

        for {
          pj <- pjs
          pk <- pks
        } {
          iSolutions.add(pj + pk)
          iSolutions.add(pk + pj)
        }
      }

      solutions.put(i, iSolutions)
    }

    solutions.get(n).get.toList
  }
}
