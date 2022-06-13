// https://leetcode.com/problems/sort-integers-by-the-number-of-1-bits/
object Solution {
  def sortByBits(arr: Array[Int]): Array[Int] = {
    arr.sortWith((a, b) => {
      val ba = bitCount(a)
      val bb = bitCount(b)
      if (ba == bb) {
        a < b
      } else {
        ba < bb
      }
    })
  }

  def bitCount(n: Int): Int = {
    var np = n
    var count = 0
    while (np != 0) {
      count += np & 1
      np >>= 1
    }
    count
  }
}
