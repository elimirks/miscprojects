// Computes the number of mergeSort for the given list

def brute(nums: Array[Int]): Int = {
  var n = 0
  for {
    i <- 0 until nums.length
    j <- i + 1 until nums.length
  } if (nums(i) > nums(j)) n += 1
  n
}

/* Performs merge, but also returns the number of elements in the left half that
 * are greater than in the right half.
 */
def merge(nums: Array[Int], p: Int, q: Int, r: Int): Int = {
  val left  = nums.slice(p, q)
  val right = nums.slice(q, r)

  var i = 0
  var j = 0
  var n = 0

  for (k <- p until r) {
    if (i < left.length && j < right.length) {
      if (left(i) < right(j)) {
        nums(k) = left(i)
        i += 1
      } else {
        nums(k) = right(j)
        j += 1
        /* Count number of inversions between the halves
         * right(j) has to "jump over" this many left elements, so that's the
         * number of inversions it has.
         */
        val leftRemaining = left.length - i 
        n += leftRemaining
      }
    } else if (i < left.length) {
      nums(k) = left(i)
      i += 1
    } else {
      nums(k) = right(j)
      j += 1
    }
  }
  //println(left.mkString(",") + " | " + right.mkString(",") + " | " + n.toString)
  n
}

def mergeSort(nums: Array[Int], p: Int, r: Int): Int = {
  if (r - p > 1) {
    val q = (p + r) / 2
    val left   = mergeSort(nums, p, q) // # inversions in left
    val right  = mergeSort(nums, q, r) // # inversions in right
    val merged = merge(nums, p, q, r)  // ???

    left + right + merged
  } else {
    0
  }
}

def sort(nums: Array[Int]): Array[Int] = {
  val inversionCount = mergeSort(nums, 0, nums.length)
  println(s"Inversion count: $inversionCount")
  nums
}

println("Brute force:")
println(brute(Array(2, 3, 8, 6, 1)))
println(brute(Array(5, 4, 3, 2, 1)))
println(brute(Array(1, 2, 3, 4, 5)))

println("Divide and conquer inversion:")
println(sort(Array(2, 3, 8, 6, 1)).mkString(","))
println(sort(Array(5, 4, 3, 2, 1)).mkString(","))
println(sort(Array(1, 2, 3, 4, 5)).mkString(","))
