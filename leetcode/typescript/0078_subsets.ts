function subsets(nums: number[]): number[][] {
  let result: number[][] = [[]];
  if (nums.length > 0) {
    result.push([nums[0]]);
    let rec = subsets(nums.slice(1)).filter((sub) => sub.length > 0);
    for (let sub of rec) {
      result.push([nums[0], ...sub]);
      result.push(sub);
    }
  }
  return result;
}
