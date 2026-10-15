// O(n log n)
function maximumProductSlow(nums: number[]): number {
  const sorted = nums.sort((a, b) => b - a);

  const min1 = sorted[sorted.length - 1];
  const min2 = sorted[sorted.length - 2];
  const max1 = sorted[0];
  const max2 = sorted[1];
  const max3 = sorted[2];

  return Math.max(
    max1 * max2 * max3,
    max1 * min1 * min2
  );
}

function maximumProduct(nums: number[]): number {
  let min1 = Infinity;
  let min2 = Infinity;
  let max1 = -Infinity;
  let max2 = -Infinity;
  let max3 = -Infinity;

  for (let n of nums) {
    if (n < min1) {
      min2 = min1;
      min1 = n;
    } else if (n < min2) {
      min2 = n;
    }

    if (n > max1) {
      max3 = max2;
      max2 = max1;
      max1 = n;
    } else if (n > max2) {
      max3 = max2;
      max2 = n;
    } else if (n > max3) {
      max3 = n;
    }
  }

  return Math.max(
    max1 * max2 * max3,
    max1 * min1 * min2
  );
}
