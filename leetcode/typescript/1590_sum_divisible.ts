function minSubarray(nums: number[], p: number): number {
  // Maps sum % p -> nums index
  let sumIndexMap = new Map<number, number[]>;
  let sums: number[] = [];
  let sum: number = 0;
  for (let i = nums.length - 1; i >= 0; i--) {
    sum = (sum + nums[i]) % p;
    sums[i] = sum;

    let entry = sumIndexMap.get(sum) || [];
    entry.push(i);
    sumIndexMap.set(sum, entry);
  }

  if (sum % p == 0) return 0;

  console.log(sumIndexMap);

  let min = nums.length;
  for (let i = 0; i < nums.length; i++) {
    // Special case: we take the entire RHS of the nums array
    if (sums[i] == sum) {
      min = Math.min(min, nums.length - i);
    }

    // sums[j] == sums[i] - sum
    // sums[j] == p + sums[i] - sum
    console.log([...(sumIndexMap[(sum - sums[i]) % p] || []), ...(sumIndexMap[(p + sum - sums[i]) % p] || [])]);

    for (let j = i + 1; j < Math.min(nums.length, i + min); j++) {
      let rangeSum = sums[i] - sums[j];
      if ((sum - rangeSum) == 0 || sum - rangeSum == p) {
        min = Math.min(min, j - i);
      }
    }
  }

  return min == nums.length ? -1 : min;
}

// Remainder approach
// function minSubarray(nums: number[], p: number): number {
//   let sums: number[] = [nums[0]];
//   for (let i = 1; i < nums.length; i++) {
//     sums[i] = nums[i] + sums[i - 1];
//   }
//
//   let sum = sums[sums.length - 1];
//   if (sum % p == 0) return 0;
//
//   let min = nums.length;
//   for (let i = 0; i < nums.length; i++) {
//     for (let j = i + 1; j < nums.length; j++) {
//       let rangeSum = sums[j] - sums[i];
//       if ((sum - rangeSum) % p == 0) {
//         min = Math.min(min, j - i);
//       }
//     }
//   }
//
//   return min == nums.length ? -1 : min;
// }
