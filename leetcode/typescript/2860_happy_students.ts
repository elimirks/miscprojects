function countWays(nums: number[]): number {
  nums = nums.sort((a, b) => a - b);

  let result = 0;
  // Special case: Select nobody
  if (nums[0] != 0) {
    result++;
  }

  for (let i = 0; i < nums.length; i++) {
    if (i + 1 > nums[i]) {
      if (i == nums.length - 1) {
        result++;
      } else if (i + 1 < nums[i + 1]) {
        result++;
      }
    }
  }

  return result;
}
