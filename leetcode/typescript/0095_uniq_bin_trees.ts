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

function gen(min: number, max: number): Array<TreeNode> {
  if (min == max) {
    return [new TreeNode(min)];
  } else if (min > max) {
    return [];
  }

  let res: Array<TreeNode> = [];

  for (let i = min; i <= max; i++) {
    let lhsSubtrees: Array<TreeNode> = gen(min, i - 1);
    let rhsSubtrees: Array<TreeNode> = gen(i + 1, max);

    let withLhsAttached: Array<TreeNode> = [];
    for (let lhsSubtree of lhsSubtrees) {
      withLhsAttached.push(new TreeNode(i, lhsSubtree));
    }
    if (withLhsAttached.length == 0) {
      withLhsAttached.push(new TreeNode(i));
    }

    if (rhsSubtrees.length == 0) {
      res = [...res, ...withLhsAttached];
    } else {
      for (let lhsAttached of withLhsAttached) {
        for (let rhs of rhsSubtrees) {
          res.push(new TreeNode(lhsAttached.val, lhsAttached.left, rhs));
        }
      }
    }
  }
  return res;
}

function generateTrees(n: number): Array<TreeNode | null> {
  return gen(1, n);
}
