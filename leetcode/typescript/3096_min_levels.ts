function minimumLevels(possible: number[]): number {
  // Reuse the input array for DP for O(1) memory footprint
  for (let i = possible.length - 1; i >= 0; i--) {
    let next = possible[i + 1] || 0;
    possible[i] = next + (possible[i] == 0 ? -1 : 1);
  }
  for (let i = 1; i < possible.length; i++) {
    if (possible[0] > 2 * possible[i]) return i;
  }
  return -1;
}
