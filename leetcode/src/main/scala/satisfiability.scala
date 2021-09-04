import scala.collection.mutable

// https://leetcode.com/problems/satisfiability-of-equality-equations/
object Solution {
  def equationsPossible(equations: Array[String]): Boolean = {
    val equalities = mutable.Map[Char, mutable.Set[Char]]()

    equations
      .filter(_.charAt(1) == '=')
      .foreach(eq => {
        val lhs = eq.charAt(0)
        val rhs = eq.charAt(3)

        val currentLhs = equalities.getOrElse(lhs, mutable.Set(lhs))
        val currentRhs = equalities.getOrElse(rhs, mutable.Set(rhs))

        val newEqualities = currentLhs.addAll(currentRhs)

        equalities.put(lhs, newEqualities)
        equalities.put(rhs, newEqualities)
      })

    equations
      .filter(_.charAt(1) == '!')
      .forall(eq => {
        val lhs = eq.charAt(0)
        val rhs = eq.charAt(3)

        val lhsSet = equalities.getOrElse(lhs, mutable.Set(lhs))
        val rhsSet = equalities.getOrElse(rhs, mutable.Set(rhs))

        lhsSet.intersect(rhsSet).size == 0
      })
  }
}
