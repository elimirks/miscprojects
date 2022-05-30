// https://leetcode.com/problems/divide-two-integers/
/*
 * What does it _mean_ to divide?
 * When performing 10/3, could it be reprhrased to:
 * "how many times can 3 be multiplied before it's greater than 10?"
 * I think the answer is yes
 */
object NaiveSolution {
  /**
    * -2^31 <= dividend, divisor <= 2^31 - 1
    * @param divisor != 0
    */
  def divide(dividend: Int, divisor: Int): Int = {
    (dividend, divisor) match {
      // Handle special cases
      case (Int.MaxValue, -1) => Int.MinValue + 1
      case (Int.MinValue, -1) => Int.MaxValue
      case _ =>
        if ((dividend < 0) ^ (divisor < 0)) {
          -safeDivide(dividend.abs, divisor.abs)
        } else {
          safeDivide(dividend.abs, divisor.abs)
        }
    }
  }

  // TODO: gotta go faster... maybe double up first?
  def safeDivide(dividend: Int, divisor: Int): Int = {
    var count = 0
    var num = divisor
    while (num <= dividend) {
      num += divisor
      count += 1
    }
    count
  }
}

/*
 * Motivation:
 * Let's construct an array of doubles of the divisor:
 * For the case, 30/3:
 * Array(3, 6, 12, 24, 48)
 * Now, we stop at 48, since it's larger than 30.
 * Begin stepping back: Is 24 larger than 30? No.
 * => Add 8 to the quotient
 * => Negate 24 from dividend
 * Continue walking backwards down the array, until you hit the beginning.
 * You'll hit the case of 6:
 * => Add 2 to the quotient
 * => Negate 6 from the dividend
 * Note that the max array size will be 33, since 2^32 > Int.MaxSize anyway
 * Fin.
 * By necessity, we cannot add the same number twice. Since in that case,
 * it would mean we _should have_ picked a larger array index first.
 * But since we're iterating from the back, that's impossible.
 *
 * NOTE: Strictly speaking, you don't have to use an array. But it's easier to think about this way
 */
object ArraySolution {
  /**
    * -2^31 <= dividend, divisor <= 2^31 - 1
    * @param divisor != 0
    */
  def divide(dividend: Int, divisor: Int): Int = {
    val absQuotient = safeDivide(dividend.toLong.abs, divisor.toLong.abs)
    val quotient = if ((dividend < 0) ^ (divisor < 0)) {
      -absQuotient
    } else {
      absQuotient
    }
    if (quotient > Int.MaxValue) {
      Int.MaxValue
    } else {
      quotient.toInt
    }
  }

  // Assumes the params are positive numbers
  def safeDivide(dividend: Long, divisor: Long): Long = {
    val storage = Array.ofDim[Long](33)
    storage(0) = divisor
    var pointer = 0
    while (storage(pointer) <= dividend) {
      storage(pointer + 1) = storage(pointer) << 1
      pointer += 1
    }

    var quotient = 0L
    var acc = dividend
    while (pointer >= 0 && acc != 0) {
      if (storage(pointer) <= acc) {
        acc -= storage(pointer)
        quotient += 1L << pointer
      }
      pointer -= 1
    }
    quotient
  }
}

// Same as ArraySolution, but using bitshifting instead of an array and pointer
object Solution {
  /**
    * -2^31 <= dividend, divisor <= 2^31 - 1
    * @param divisor != 0
    */
  def divide(dividend: Int, divisor: Int): Int = {
    val absQuotient = safeDivide(dividend.toLong.abs, divisor.toLong.abs)
    val quotient = if ((dividend < 0) ^ (divisor < 0)) {
      -absQuotient
    } else {
      absQuotient
    }
    if (quotient > Int.MaxValue) {
      Int.MaxValue
    } else {
      quotient.toInt
    }
  }

  // Assumes the params are positive numbers
  def safeDivide(dividend: Long, divisor: Long): Long = {
    var doubled = divisor
    var pointer = 0
    while (doubled <= dividend) {
      doubled <<= 1
      pointer += 1
    }
    var quotient = 0L
    var acc = dividend
    while (pointer >= 0 && acc != 0) {
      if (doubled <= acc) {
        acc -= doubled
        quotient += 1L << pointer
      }
      pointer -= 1
      doubled >>= 1
    }
    quotient
  }
}

println(Solution.divide(10, 3))
println(Solution.divide(-7, 3))
println(Solution.divide(0, 3))
println(Solution.divide(1, 3))
println(Solution.divide(2, 3))
println(Solution.divide(3, 3))
println(Solution.divide(4, 3))
println(Solution.divide(-2147483648, -1))
println(Solution.divide(-2147483648, 1))
