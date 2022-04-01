package problem415

import scala.collection.{mutable => mut}

import problem413.{
  maxSubarrayDnc,
  maxSubarrayBrute,
  maxSubarrayHybrid,
  benchmark,
  generateArray
}

def testSolutions(): Unit = {
  for (i <- 0 until 10000) {
    val testArray = generateArray(i, 32)
    val (_, _, dnc) = maxSubarrayDnc(testArray, 0, testArray.size - 1)
    val (_, _, linear) = maxSubarrayLinear(testArray, 0, testArray.size - 1)
    assert(dnc == dnc)
  }
}

/** Algorithm motivation: A max subarray of A[0..j+1] is either a max subarray
  * of A[0..j] or a subarray of A[i..j+1], for some 1 <= i <= j + 1. Determine a
  * max subarray of the form A[i..j+1] in constant time based on knowing the a
  * maximum subarray ending at index j.
  * @return
  *   (lower, upper, sum)
  */
def maxSubarrayLinear(
    nums: Array[Int],
    low: Int,
    high: Int
): (Int, Int, Int) = {
  // Stores max subarray ending at `j` for (i, sum), where j is the array index
  val dyn = Array.ofDim[(Int, Int)](nums.size)
  dyn(0) = (0, nums(0))
  var max = (0, 0, nums(0))

  for (i <- 1 until nums.size) {
    val (prevI, prevSum) = dyn(i - 1)
    dyn(i) =
      if (prevSum < 0)
        (i, nums(i))
      else
        (prevI, prevSum + nums(i))

    if (max._3 < dyn(i)._2) {
      max = (dyn(i)._1, i, dyn(i)._2)
    }
  }
  max
}

@main def entry(): Unit = {
  testSolutions()

  Seq(1, 2, 5, 10, 15, 20, 100, 500, 10000).foreach(size => {
    println(s"Size:   $size")
    println(s"Hybrid: ${benchmark(size, maxSubarrayHybrid)}")
    println(s"DNC:    ${benchmark(size, maxSubarrayDnc)}")
    if (size < 1000) println(s"Brute:  ${benchmark(size, maxSubarrayBrute)}")
    println(s"Linear: ${benchmark(size, maxSubarrayLinear)}")
    println(s"--------------------")
  })
}
