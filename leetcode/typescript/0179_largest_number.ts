function largestNumber(nums: number[]): string {
  return nums.map(String)
    .sort((a, b) => parseInt(b + a) - parseInt(a + b))
    .reduce((acc, it) => {
      if (acc == "0") return it;
      return `${acc}${it}`;
    });
}
