package util

def time[R](block: => R): Long = {
  val t0 = System.nanoTime()
  block
  val t1 = System.nanoTime()
  t1 - t0
}
