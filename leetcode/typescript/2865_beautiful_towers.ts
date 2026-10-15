function maximumSumOfHeights(heights: number[]): number {
  let increasingDyn: number[] = [heights[0]];
  for (let i = 1; i < heights.length; i++) {
    let j = i - 1;
    for (; j >= 0 && heights[j] > heights[i]; j--);

    if (j == -1) {
      increasingDyn[i] = heights[i] * (1 + i);
    } else {
      increasingDyn[i] = heights[i] * (i - j) + increasingDyn[j];
    }
  }

  let decreasingDyn: number[] = [];
  decreasingDyn[heights.length - 1] = heights[heights.length - 1];
  for (let i = heights.length - 2; i >= 0; i--) {
    let j = i + 1;
    for (; j <= heights.length - 1 && heights[j] > heights[i]; j++);

    if (j == heights.length) {
      decreasingDyn[i] = heights[i] * (heights.length - i);
    } else {
      decreasingDyn[i] = heights[i] * (j - i) + decreasingDyn[j];
    }
  }

  let result = 0;

  for (let i = 0; i < heights.length; i++) {
    result = Math.max(result, increasingDyn[i] + decreasingDyn[i] - heights[i]);
  }

  return result;
}
