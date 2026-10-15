// Not super efficient. Pretty wasteful checking isIncreasing all the time

// Assumes `start` and `end` are in bounds
function isIncreasing(nums: number[], start: number, end: number): boolean {
  if (start == end) return true;
  let cur = nums[start];
  for (let i = start + 1; i <= end; i++) {
    if (nums[i] <= cur) return false;
    cur = nums[i];
  }
  return true;
}

function findLengthOfLCIS(nums: number[]): number {
  let size = 1;
  let idx = 0;
  while (idx + size < nums.length) {
    if (isIncreasing(nums, idx, idx + size)) {
      size++;
    } else {
      idx++;
    }
  }
  return size;
}
