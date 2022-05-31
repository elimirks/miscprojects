// https://leetcode.com/problems/missing-number/
import scala.collection.mutable

// Not great solution, O(n) memory, O(n) runtime
object SimpleSolution {
  def missingNumber(nums: Array[Int]): Int = {
    val expectedNums = mutable.Set[Int]()
    expectedNums.addAll(0 to nums.size)
    for (n <- nums) {
      expectedNums.remove(n)
    }
    expectedNums.head
  }
}

/*
 * Another solution is to sort, then look for gaps.
 * But can we sort in O(n)? In this case, yes!
 *
 * What information do I have?
 * I know that there is an ordering to nums.
 * What if, I attempte to sort the numbers by swapping? I can do it since
 * I know their order already! The highest num can be a special case...
 * I can swap it out for a -1, placeholder token
 * It's GUARANTEED that a number won't be displaced once it's moved! Since there
 * is no other contender for that spot.
 * At the end of the operation, -1 will be in the spot where the missing number is
 */
object SortingSolution {
  def missingNumber(nums: Array[Int]): Int = {
    // "extra spot" to move the highest number into
    var bonus = -1
    // Keep swapping the current spot until it's in the right order
    // ... or using the placeholder value
    for (i <- 0 until nums.size) {
      while (nums(i) != -1 && nums(i) != i) {
        val n = nums(i)
        if (n == nums.size) {
          bonus = n
          nums(i) = -1
        } else {
          val n = nums(i)
          nums(i) = nums(n)
          nums(n) = n
        }
      }
    }
    if (bonus == -1) nums.size else nums.indexOf(-1)
  }
}

// Solution by negating each number from the expected total
object Solution {
  def missingNumber(nums: Array[Int]): Int = {
    val n = nums.size
    nums.foldLeft(n * (n + 1) / 2)(_ - _)
  }
}

println(Solution.missingNumber(Array(1, 2)))
println(Solution.missingNumber(Array(3, 0, 1)))
println(Solution.missingNumber(Array(0, 1)))
println(Solution.missingNumber(Array(9,6,4,2,3,5,7,0,1)))
