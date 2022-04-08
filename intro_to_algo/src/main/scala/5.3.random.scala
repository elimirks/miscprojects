package problem53

import scala.collection.{mutable => mut}
import scala.util.Random
import util.{mean, stdDev}

val r = Random()

/**
  * Provably uniform distribution of an in place array shuffle.
  * The book shows a proper mathematical proof.
  * But findDist shows some computational evidence that it's uniform.
  */
def randomizeInPlace(nums: Array[Int]): Array[Int] = {
  val result = nums.clone
  for (i <- 0 until result.length - 1) {
    val tmp = result(i)
    val n = i + r.nextInt(result.length - i)
    result(i) = result(n)
    result(n) = tmp
  }
  result
}

def findDist(f: Array[Int] => Array[Int]): Map[String, Int] = {
  val nums = (1 to 5).toArray
  val dist = mut.Map[String, Int]()

  for (_ <- 0 until 1000000) {
    val result = randomizeInPlace(nums)
    val resultStr = result.mkString(",")
    dist.put(resultStr, 1 + dist.getOrElse(resultStr, 0))
  }

  val distMax = dist.values.max
  val distMin = dist.values.min
  val distStd = stdDev(dist.values)
  val distMean = mean(dist.values)

  println(s"Distribution distMax:   $distMax")
  println(s"Distribution distMin:   $distMin")
  println(s"Standard deviation: $distStd (${distStd / distMean}% of mean)")

  dist.toMap
}

@main def entry(): Unit = {
  println("Computing distribution of randomizeInPlace")
  println("==========================================")
  findDist(randomizeInPlace)
}
