// https://leetcode.com/problems/number-of-steps-to-reduce-a-number-to-zero/

// Trivial solution... is there a trick to do better though?
object TrivialSolution {
  def numberOfSteps(num: Int): Int = {
    var acc = num
    var steps = 0
    while (acc != 0) {
      if (acc % 2 == 0) {
        acc /= 2
      } else {
        acc -= 1
      }
      steps += 1
    }
    steps
  }
}
