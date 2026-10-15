function minSwaps(nums: number[]): number {
  const oneCount = nums.reduce((acc, it) => acc + it, 0);
  if (oneCount == nums.length || oneCount == 0) return 0;

  let windowSum: number = 0;
  for (let i = 0; i < oneCount; i++) {
    windowSum += nums[i];
  }

  let maxSum = windowSum;

  for (let i = oneCount; i < nums.length + oneCount; i++) {
    const next = nums[i % nums.length];
    const prev = nums[(nums.length + i - oneCount) % nums.length];

    windowSum -= prev;
    windowSum += next;

    maxSum = Math.max(maxSum, windowSum);
  }

  return oneCount - maxSum;
}
