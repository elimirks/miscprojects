// https://leetcode.com/problems/number-of-ways-where-square-of-number-is-equal-to-product-of-two-numbers/
object Solution {
  // Type 1: Triplet (i, j, k) if nums1[i]^2 == nums2[j] * nums2[k]
  // Type 2: Triplet (i, j, k) if nums2[i]^2 == nums1[j] * nums1[k]
  def numTriplets(nums1: Array[Int], nums2: Array[Int]): Int = {
    val lnums1 = nums1.map(_.toLong)
    val lnums2 = nums2.map(_.toLong)
    trip(lnums1, lnums2) + trip(lnums2, lnums1)
  }

  def trip(lhs: Array[Long], rhs: Array[Long]): Int = {
    val lhsSquares = lhs
      .groupBy(n => n * n)
      .mapValues(_.size)

    (0 until rhs.length).flatMap(j => {
      (j + 1 until rhs.length).map(k => {
        lhsSquares.getOrElse(rhs(j) * rhs(k), 0)
      })
    }).sum
  }
}
