function findMedian(nums: number[]): number {
  if (nums.length === 0) {
    throw "Invalid input";
  }
  if (nums.length % 2 === 0) {
    return (nums[nums.length / 2] + nums[nums.length / 2 - 1]) / 2;
  } else {
    return nums[Math.floor(nums.length / 2)];
  }
}

function findMedianSortedArrays(nums1: number[], nums2: number[]): number {
  console.log(nums1, nums2);
  // Binary search, bouncing betwees the two arrays
  // Cases:
  // 1. nums1 < nums2 (easy)
  // 2. nums2 > nums1 (easy)
  // 3. mixed
  //
  // For the mixed case, I'm thinking we can split each array in half, then recurse
  // If we take an equal number away from the lhs of nums1 and rhs of nums2, the median should _stay the same_

  // Base cases: nums1 or nums2 is empty, or there is only a single value in both arrays
  if (nums1.length == 0) return findMedian(nums2);
  if (nums2.length == 0) return findMedian(nums1);
  if (nums1.length == 1 && nums2.length == 1) return (nums1[0] + nums2[0]) / 2;

  const maxDrop = Math.min(nums1.length, nums2.length);
  const dropAmount = Math.min(maxDrop, Math.max(Math.floor(nums1.length / 2), Math.floor(nums2.length / 2)));

  if (nums1[0] < nums2[0]) {
    const lhs = nums1.slice(dropAmount);
    const rhs = nums2.slice(0, nums2.length - dropAmount);
    return findMedianSortedArrays(lhs, rhs);
  } else {
    const lhs = nums2.slice(dropAmount);
    const rhs = nums1.slice(0, nums1.length - dropAmount);
    return findMedianSortedArrays(lhs, rhs);
  }
}
