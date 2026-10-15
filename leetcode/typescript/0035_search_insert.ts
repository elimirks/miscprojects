function searchInsert(nums: number[], target: number): number {
  if (nums.length == 0) {
    return 0;
  }
  let startIdx = 0;
  let endIdx = nums.length - 1;

  while (startIdx < endIdx - 1) {
    const midIdx = Math.floor((endIdx + startIdx) / 2);
    const midVal = nums[midIdx];

    if (midVal == target) {
      return midIdx;
    } else if (midVal > target) {
      endIdx = midIdx;
    } else {
      startIdx = midIdx;
    }
  }

  if (nums[endIdx] < target) return endIdx + 1;
  if (nums[startIdx] >= target) return startIdx;
  return endIdx;
}



// Recursive solution - not quite right
// function searchInsert(nums: number[], target: number): number {
//   if (nums.length == 0) {
//     return 0;
//   }
//   if (nums.length == 1) {
//     if (nums[0] > target) {
//       return 0;
//     } else {
//       return 1;
//     }
//   }
//
//   const midIdx = Math.floor((nums.length - 1) / 2);
//   const mid = nums[midIdx];
//
//   if (target == mid) {
//     return midIdx;
//   } else if (target < mid) {
//     return searchInsert(nums.slice(0, midIdx), target);
//   } else {
//     return 1 + midIdx + searchInsert(nums.slice(midIdx + 1), target);
//   }
// }
