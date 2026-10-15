function maxDistance(arrays: number[][]): number {
  // Step 1: Compute minimum values looking left and right - O(n)

  let lhsMins: number[] = [arrays[0][0]];
  for (let i = 1; i < arrays.length; i++) {
    lhsMins[i] = Math.min(lhsMins[i - 1], arrays[i][0]);
  }

  let rhsMins: number[] = [];
  rhsMins[arrays.length - 1] = arrays[arrays.length - 1][0];
  for (let i = arrays.length - 2; i >= 0; i--) {
    rhsMins[i] = Math.min(rhsMins[i + 1], arrays[i][0]);
  }

  // Step 2: Assume the max value at each index, and look at the rhs and lhs mins to compute the delta
  let largest = 0;
  for (let i = 0; i < arrays.length; i++) {
    const iMax = arrays[i][arrays[i].length - 1];

    if (i > 0) {
      largest = Math.max(largest, Math.abs(iMax - lhsMins[i - 1]));
    }
    if (i < arrays.length - 1) {
      largest = Math.max(largest, Math.abs(iMax - rhsMins[i + 1]));
    }
  }
  return largest;
}
