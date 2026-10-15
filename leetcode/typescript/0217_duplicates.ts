function containsDuplicate(nums: number[]): boolean {
  let set = new Set<number>();
  for (let n of nums) {
    if (set.has(n)) return true;
    set.add(n);
  }
  return false;
}
