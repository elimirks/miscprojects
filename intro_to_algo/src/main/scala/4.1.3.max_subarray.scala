package problem413

import scala.util.Random

/**
  * Generates an array of random numbers in the given bounds.
  */
def generateArray(
  seed: Int,
  size: Int,
  min: Int = -100,
  max: Int = 100
): Array[Int] = {
  val rng = Random(seed)
  (0 until size).map(_ => rng.nextInt(max - min) + min).toArray
}

/**
  * @return (lower, upper, sum)
  */
def maxSubarrayBrute(
  nums: Array[Int],
  low: Int,
  high: Int
): (Int, Int, Int) = {
  var max = Int.MinValue
  var lower = 0
  var upper = 0 
  for (i <- low to high) {
    var sum = 0

    for (j <- i to high) {
      sum += nums(j)

      if (sum > max) {
        max = sum
        lower = i
        upper = j
      }
    }
  }
  (lower, upper, max)
}

/**
  * Finds the max subarray that crosses a given boundary
  * Expects low < mid < high (nonequal)
  * @return (lower, upper, sum)
  */
def findMaxCrossingSubarray(
  nums: Array[Int],
  low: Int,
  mid: Int,
  high: Int
): (Int, Int, Int) = {
  var leftSum = Int.MinValue
  var sum = 0
  // These two values can be arbitrary since we expect the lhs and rhs to be
  // nonempty arrays
  var maxLeft = 0
  var maxRight = 0

  for (i <- (low to mid).reverse)
    sum = sum + nums(i)
    if (sum > leftSum)
      leftSum = sum
      maxLeft = i

  var rightSum = Int.MinValue
  sum = 0

  for (i <- (mid + 1) to high)
    sum = sum + nums(i)
    if (sum > rightSum)
      rightSum = sum
      maxRight = i

  (maxLeft, maxRight, leftSum + rightSum)
}

// Divide & conquer solution
def maxSubarrayDnc(
  nums: Array[Int],
  low: Int,
  high: Int
): (Int, Int, Int) = {
  if (low == high) {
    (low, high, nums(low))
  } else {
    val mid = (low + high) / 2
    val (leftLow, leftHigh, leftSum) = maxSubarrayDnc(nums, low, mid)
    val (rightLow, rightHigh, rightSum) = maxSubarrayDnc(nums, mid + 1, high)
    val (crossLow, crossHigh, crossSum) = findMaxCrossingSubarray(nums, low, mid, high)

    if (leftSum >= rightSum && leftSum >= crossSum) {
      (leftLow, leftHigh, leftSum)
    } else if (rightSum >= crossSum) {
      (rightLow, rightHigh, rightSum)
    } else {
      (crossLow, crossHigh, crossSum)
    }
  }
}

/**
  * Based on the point from "findCrossover" running multiple times
  * Running `findCrossover` once won't really work, since the JIT doesn't have time.
  */
def maxSubarrayHybrid(
  nums: Array[Int],
  low: Int,
  high: Int
): (Int, Int, Int) = {
  if (high - low < 18) {
    maxSubarrayBrute(nums, low, high)
  } else {
    val mid = (low + high) / 2
    val (leftLow, leftHigh, leftSum) = maxSubarrayDnc(nums, low, mid)
    val (rightLow, rightHigh, rightSum) = maxSubarrayDnc(nums, mid + 1, high)
    val (crossLow, crossHigh, crossSum) = findMaxCrossingSubarray(nums, low, mid, high)

    if (leftSum >= rightSum && leftSum >= crossSum) {
      (leftLow, leftHigh, leftSum)
    } else if (rightSum >= crossSum) {
      (rightLow, rightHigh, rightSum)
    } else {
      (crossLow, crossHigh, crossSum)
    }
  }
}

def testSolutions(): Unit = {
  for (i <- 0 until 10000) {
    val testArray = generateArray(i, 32)
    val (_, _, brute)  = maxSubarrayBrute(testArray, 0, testArray.size - 1)
    val (_, _, dnc)    = maxSubarrayDnc(testArray, 0, testArray.size - 1)
    val (_, _, hybrid) = maxSubarrayHybrid(testArray, 0, testArray.size - 1)
    assert(brute == dnc)
    assert(brute == hybrid)
  }
}

def time[R](block: => R): Long = {
  val t0 = System.nanoTime()
  block
  val t1 = System.nanoTime()
  t1 - t0
}

type SolFun = (Array[Int], Int, Int) => (Int, Int, Int)

def benchmark(size: Int, maxSubarray: SolFun): Long = {
  var total = 0L
  for (i <- 0 until 100) {
    val testArray = generateArray(i, size)
    total += time {
      maxSubarray(testArray, 0, testArray.size - 1)
    }
  }
  total
}

/**
  * Finds the crossover in runtime between two function input sizes
  */
def findCrossover(start: Int, f1: SolFun, f2: SolFun): Int = {
  var i = start
  while (benchmark(i, f1) < benchmark(i, f2)) i += 1
  i
}

@main def entry(): Unit = {
  testSolutions()

  // Interestingly enough, the performance of the hybrid solution is
  // basically the same as the pure DNC solution:
  Seq(1, 2, 5, 10, 15, 20, 100, 500, 10000).foreach(size => {
    println(s"Size:   $size")
    println(s"Hybrid: ${benchmark(size, maxSubarrayHybrid)}")
    println(s"DNC:    ${benchmark(size, maxSubarrayDnc)}")
    if (size < 1000) println(s"Brute:  ${benchmark(size, maxSubarrayBrute)}")
    println(s"--------------------")
  })

  // for (i <- 0 until 20) {
  //   val crossSize = findCrossover(5, maxSubarrayBrute, maxSubarrayDnc)
  //   println(s"Crossover size: $crossSize")
  // }
}
