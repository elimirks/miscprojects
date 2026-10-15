function findNonMinOrMax(nums: number[]): number {
  let min = Math.min(...nums);
  let max = Math.max(...nums);
  for (let n of nums) {
    if (n != min && n != max) return n;
  }
  return -1
}
