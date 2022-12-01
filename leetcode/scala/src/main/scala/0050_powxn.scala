import scala.collection.mutable
// DP solution
object DpSolution {
  def myPow(x: Double, n: Int): Double = {
    n match {
      case 0           => 1.0
      case -2147483648 => 1.0 / (pow(x, 2147483647) * x) // annoying special case
      case n if n < 0  => 1.0 / pow(x, -n)
      case _           => pow(x, n)
    }
  }

  // Assumes n >= 1
  def pow(x: Double, n: Int): Double = {
    val dpMap = mutable.HashMap[Int, Double](
      0 -> 1.0,
      1 -> x,
    )
    val toCompute = mutable.Stack[Int](n)
    while (toCompute.nonEmpty) {
      val next = toCompute.pop
      val lhs = next / 2
      val rhs = next - lhs

      if (!dpMap.contains(lhs)) {
        toCompute.push(next)
        toCompute.push(lhs)
      } else if (!dpMap.contains(rhs)) {
        toCompute.push(next)
        toCompute.push(rhs)
      } else {
        dpMap.put(next, dpMap.get(lhs).get * dpMap.get(rhs).get)
      }
    }
    dpMap.get(n).get
  }
}

println(Solution.myPow(2.0, 5))
println(Solution.myPow(2.1, 3))
println(Solution.myPow(2.0, -2))
