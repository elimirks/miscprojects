// Idea: Use a tree to represent sums of the array
// Updating will take O(log n) time since it just needs to traverse the parents
// Querying will take O(log n) as well since we're only traversing up from the leaves

class NumArray {
  sumTree: number[];
  // Nodes reserved to store sums
  sumNodeCount: number;

  constructor(nums: number[]) {
    let depth = (nums.length == 1) ? 1 : Math.ceil(Math.log2(nums.length));
    this.sumNodeCount = (1 << depth) - 1;
    this.sumTree = new Array(this.sumNodeCount).fill(0);
    for (let i = 0; i < nums.length; i++) {
      this.sumTree.push(nums[i]);
    }
    for (let i = this.sumNodeCount - 1; i >= 0; i--) {
      let left = this.sumTree[(i << 1) + 1] || 0;
      let right = this.sumTree[(i << 1) + 2] || 0;
      this.sumTree[i] = left + right;
    }
  }

  updateSumTree(index: number): void {
    while (index >= 0) {
      let left = this.sumTree[(index << 1) + 1] || 0;
      let right = this.sumTree[(index << 1) + 2] || 0;
      this.sumTree[index] = left + right;

      if (index == 0) {
        break;
      }
      index = (index - 1) >> 1;
    }
  }

  update(index: number, val: number): void {
    this.sumTree[this.sumNodeCount + index] = val;
    this.updateSumTree((this.sumNodeCount + index - 1) >> 1);
  }

  sumRange(left: number, right: number): number {
    let excluded = 0;

    left += this.sumNodeCount;
    right += this.sumNodeCount;

    while (left != right) {
      if ((left & 1) == 0) {
        excluded += this.sumTree[left - 1] || 0;
      }
      if ((right & 1) == 1) {
        excluded += this.sumTree[right + 1] || 0;
      }
      left = (left - 1) >> 1;
      right = (right - 1) >> 1;
    }

    return this.sumTree[left] - excluded;
  }
}
