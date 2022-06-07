// https://leetcode.com/problems/merge-sorted-array/
/* Approach:
 * Iterate over nums1 BACKWARDS! If nums2 selection is exhausted, it's still
 * guaranteed that we won't accidentally overlap something in nums1 that we
 * shouldn't have.
 */
object Solution {
  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
    // Pointers to the ends of data in each array
    var i1 = m - 1
    var i2 = n - 1
    // Pointer the current point where we should place the highest element
    var highest = m + n - 1

    while (i1 >= 0 && i2 >= 0) {
      if (nums1(i1) > nums2(i2)) {
        nums1(highest) = nums1(i1)
        i1 -= 1
      } else {
        nums1(highest) = nums2(i2)
        i2 -= 1
      }
      highest -= 1
    }
    // Cleanup loop, for if the first array is exhausted first
    while (i2 >= 0) {
      nums1(highest) = nums2(i2)
      highest -= 1
      i2 -= 1
    }
  }
}
