function twoSum(nums: number[], target: number): number[] {
  let dp = new Map<number, number>();
  for (let i = 0; i < nums.length; i++) {
    dp.set(target - nums[i], i);
  }
  for (let i = 0; i < nums.length; i++) {
    let j = dp.get(nums[i]);
    if (j !== undefined && i !== j) {
      return [i, j];
    }
  }
  return [0, 0];
}
