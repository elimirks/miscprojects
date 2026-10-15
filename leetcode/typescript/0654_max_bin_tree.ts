class TreeNode {
    val: number
    left: TreeNode | null
    right: TreeNode | null
    constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.left = (left===undefined ? null : left)
        this.right = (right===undefined ? null : right)
    }
}

function constructMaximumBinaryTree(nums: number[]): TreeNode | null {
  if (nums.length == 0) return null;
  if (nums.length == 1) return new TreeNode(nums[0]);

  let max = nums.reduce((a, b) => Math.max(a, b), nums[0]);
  let idx = nums.indexOf(max);
  let lhs = nums.slice(0, idx);
  let rhs = nums.slice(idx + 1, nums.length);

  return new TreeNode(max, constructMaximumBinaryTree(lhs), constructMaximumBinaryTree(rhs));
}
