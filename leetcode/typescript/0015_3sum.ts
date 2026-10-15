interface Pair {
  i: number;
  j: number;
}

function threeSum(nums: number[]): number[][] {
  // Maps (- ni - nj) -> [i, j]
  let dp = new Map<number, Pair[]>();
  for (let i = 0; i < nums.length; i++) {
    for (let j = i + 1; j < nums.length; j++) {
      let idx = -nums[i] - nums[j];
      let cur = dp.get(idx);
      if (cur !== undefined) {
        dp.set(idx, [...cur, { i, j }]);
      } else {
        dp.set(idx, [{ i, j }]);
      }
    }
  }
  let results: number[][] = [];

  for (let k = 0; k < nums.length; k++) {
    let pairs = dp.get(k);
    if (!pairs) continue;

    for (let pair of pairs) {
      if (pair.i != k && pair.j != k) {
        results.push([nums[pair.i], nums[pair.j], nums[k]]);
      }
    }
  }

  return results;
}
