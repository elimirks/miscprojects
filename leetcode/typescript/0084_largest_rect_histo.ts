function largestRectangleArea(heights: number[]): number {
  let fitToLeft: number[] = new Array(heights.length).fill(0);
  let fitToRight: number[] = new Array(heights.length).fill(0);

  // Step 1: Find the number of bars that "fit" for the current index to the left
  for (let i = 0; i < heights.length; i++) {
    let j = i - 1;
    while (j >= 0 && heights[j] >= heights[i]) {
      fitToLeft[i] += fitToLeft[j] + 1;
      j -= fitToLeft[j] + 1;
    }
  }

  // Step 2: Same, but for the right side this time
  for (let i = heights.length - 1; i >= 0; i--) {
    let j = i + 1;
    while (j < heights.length && heights[j] >= heights[i]) {
      fitToRight[i] += fitToRight[j] + 1;
      j += fitToRight[j] + 1;
    }
  }

  let result = 0;
  for (let i = 0; i < heights.length; i++) {
    result = Math.max(result, heights[i] * (1 + fitToLeft[i] + fitToRight[i]));
  }
  return result;
}
