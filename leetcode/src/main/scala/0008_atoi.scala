// https://leetcode.com/problems/string-to-integer-atoi/
object Solution {
  def myAtoi(s: String): Int = {
    val trimmed = s.trim

    trimmed.headOption match {
      case Some('-') => atoiNegative(trimmed.substring(1))
      case Some('+') => atoiPositive(trimmed.substring(1))
      case Some(_)   => atoiPositive(trimmed)
      case None      => 0
    }
  }

  def atoiPositive(s: String): Int = {
    val n = atol(s)

    if (n > Int.MaxValue) {
      Int.MaxValue
    } else {
      n.toInt
    }
  }

  def atoiNegative(s: String): Int = {
    val n = -atol(s)

    if (n < Int.MinValue) {
      Int.MinValue
    } else {
      n.toInt
    }
  }

  def clean(s: String): String = {
    s
      .dropWhile(_ == '0')
      .takeWhile(c => {
        c.toInt >= '0'.toInt && c.toInt <= '9'.toInt
      })
  }

  def atol(s: String): Long = {
    val cleanS = clean(s)

    if (cleanS.length >= 12) {
      Int.MaxValue.toLong + 1L
    } else {
      cleanS.reverse.zipWithIndex
        .map({ case (c, index) =>
          val exp = math.pow(10, index).toLong
          val value = c.toLong - '0'.toLong
          value * exp
        })
        .sum
    }
  }
}
