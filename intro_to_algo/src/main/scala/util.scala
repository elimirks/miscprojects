package util

import Numeric.Implicits._

def time[R](block: => R): Long = {
  val t0 = System.nanoTime()
  block
  val t1 = System.nanoTime()
  t1 - t0
}

def mean[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size

def variance[T: Numeric](xs: Iterable[T]): Double = {
  val avg = mean(xs)
  xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size
}

def stdDev[T: Numeric](xs: Iterable[T]): Double = math.sqrt(variance(xs))
