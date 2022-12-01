// Input: nums = [2,7,11,15], target = 9
// Output: [0,1]
// Output: Because nums[0] + nums[1] == 9, we return [0, 1].
// https://leetcode.com/problems/two-sum/
object Solution {
  // O(n)
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    // O(1)
    val numIndices = nums.zipWithIndex

    // O(n)
    val deltae = numIndices.map({ case (n, index) =>
      (target - n) -> index
    }).toMap

    // O(n)
    val second = numIndices.find({ case (n, index2) =>
      deltae.get(n) match {
        case Some(index1) => index1 != index2
        case None         => false
      }
    })

    // O(1)
    val answer = for {
      (n2, index2) <- second
      index1       <- deltae.get(n2)
    } yield Array(index1, index2)

    // O(1)
    answer.getOrElse(Array())
  }
}
