package problem51

import util.time
import scala.collection.{mutable => mut}
import scala.util.Random
import scala.annotation.tailrec

val r = Random()

// Randomly returns 0 or 1
def rand01(): Int = r.nextInt.abs % 2

// Returns 0 or 1, biased with the given probability
def rand01Biased(p: Double): Int = if (r.nextDouble < p) 1 else 0

/**
  * Generated numbers between `a` and `b` using only rand01
  * @param a Must be less than or equal to b
  *
  * The approach here is a "randomized BST", so O(lg(b - a))
  * Unfortunately, the distribution isn't uniform.
  */
@tailrec
def rand512(a: Int, b: Int): Int = {
  assert(b > a)

  if (a == b) {
    a
  } else if (a == b - 1) {
    a + rand01()
  } else {
    // The recursive case only works when delta >= 2
    val delta = (b - a) / 2
    if (rand01() == 0) {
      rand512(a, b - delta)
    } else {
      rand512(a + delta, b)
    }
  }
}

/**
  * Generates uniform distribution of 0s and 1s based on a biased random func.
  */
def rand513Recursive(p: Double): Int = {
  @tailrec
  def f(lefty: Double, righty: Double): Int = {
    val cursor = p * (righty - lefty) + lefty
    val n = rand01Biased(p)

    if (cursor < 0.5) {
      if (n == 1) return 1
      f(cursor, righty)
    } else {
      if (n == 0) return 0
      f(lefty, cursor)
    }
  }
  f(0.0, 1.0)
}

def rand513Invert(p: Double): Int = {
  while (true) {
    val a = rand01Biased(p)
    val b = 1 - rand01Biased(p)
    // ab = choice of 00, 11, 01, 10
    // 90% to get a 1
    //  00 -> .1 * .9 = .09
    //  11 -> .9 * .1 = .09
    //  01 -> .1 * .1 = .01
    //  10 -> .9 * .9 = .81
    if (a == b) return a
  }
  ???
}

def sampleRngFunction(gen: () => Int): Map[Int, Int] = {
  val distMap = mut.Map[Int, Int]()
  for (_ <- 0 until 100000) {
    val n = gen()
    distMap.put(n, distMap.getOrElse(n, 0) + 1)
  }
  distMap.toMap
}

def benchmark(gen: Double => Int): Long = time {
  val max = 100
  for {
    i <- 1 until max
    _ <- 0 until 10000
  } {
    gen(i.toDouble / max.toDouble)
  }
}

@main def entry(): Unit = {
  // println(sampleRngFunction(rand01))
  // println(sampleRngFunction(() => rand512(0, 7)))
  // println(sampleRngFunction(() => rand512(5, 10)))
  // println(sampleRngFunction(() => rand513Recursive(0.1)))
  // println(sampleRngFunction(() => rand513Recursive(0.2)))
  // println(sampleRngFunction(() => rand513Recursive(0.3)))
  // println(sampleRngFunction(() => rand513Recursive(0.4)))
  // println(sampleRngFunction(() => rand513Recursive(0.5)))
  // println(sampleRngFunction(() => rand513Recursive(0.6)))
  // println(sampleRngFunction(() => rand513Recursive(0.7)))
  // println(sampleRngFunction(() => rand513Recursive(0.8)))
  // println(sampleRngFunction(() => rand513Recursive(0.9)))
  println(sampleRngFunction(() => rand513Recursive(0.999)))
  println(benchmark(rand513Recursive))
  println(benchmark(rand513Invert))
}
