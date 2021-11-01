import scala.collection.{mutable => mut}
// https://leetcode.com/problems/shifting-letters/
object Solution {
  def shiftingLetters(s: String, shifts: Array[Int]): String = {
    val shiftedValues = mut.ArrayBuffer.fill(s.size)('\u0000')
    var totalShift = 0L

    // Go from right to left so we can accumulate totalShift
    for (i <- (0 until shifts.size).reverse) {
      totalShift += shifts(i)
      val prev = s.charAt(i).toLong - 'a'.toLong
      val wrapped = (prev + totalShift) % 26
      shiftedValues(i) = (wrapped + 'a'.toLong).toChar
    }

    shiftedValues.mkString
  }
}

println(Solution.shiftingLetters("abc", Array(3, 5, 9)))
