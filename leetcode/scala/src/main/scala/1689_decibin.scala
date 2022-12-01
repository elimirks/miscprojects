// https://leetcode.com/problems/partitioning-into-minimum-number-of-deci-binary-numbers/
// Why is this marked as a medium problem???

object Solution {
  def minPartitions(n: String): Int = {
    n.max.toInt - '0'.toInt
  }
}
