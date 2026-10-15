function longestSquareStreak(nums: number[]): number {
  // Stores numbers we have the square of (in `nums`)
  let hasSquareOf = new Set<number>();

  for (let n of nums) {
    let sqrt = Math.sqrt(n);
    if (sqrt % 1 == 0) {
      hasSquareOf.add(sqrt);
    }
  }

  let max = -1;
  for (let n of nums) {
    let count = 1;
    while (hasSquareOf.has(n)) {
      count += 1;
      n = n * n;
    }
    if (count >= 2 && count > max) {
      max = count
    }
  }
  return max;
}
