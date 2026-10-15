function sumOfPowers(nums: number[], k: number): number {
  // Powers of each 2-subsequence. Only calculated once at O(n^2) for better efficiency
  let twoPowerMatrix: number[][] = [];
  for (let i = 0; i < nums.length; i++) {
    twoPowerMatrix.push([]);
  }

  for (let i = 0; i < nums.length; i++) {
    twoPowerMatrix[i][i] = 0;

    for (let j = i + 1; j < nums.length; j++) {
      twoPowerMatrix[i][j] = Math.abs(nums[i] - nums[j]);
      twoPowerMatrix[j][i] = twoPowerMatrix[i][j];
    }
  }

  // The naive solution is to create a hypermatrix, at O(n^k) memory and runtime
  // Or we take an n choose k approach: O(n!(k!(n-k)!)) runtime, O(n^2) memory
  // Yuck.. factorial runtime complexity


  console.log(twoPowerMatrix);

  // Solution 2: incrementally find powers for each k value 
  return 0;
}
