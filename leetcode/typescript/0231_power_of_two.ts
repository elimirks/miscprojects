function isPowerOfTwo(n: number): boolean {
  return n != 0 && Math.log2(n) == Math.floor(Math.log2(n));
}
