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

interface QueueEntry {
  node: TreeNode,
  depth: number,
}

function levelOrder(root: TreeNode | null): number[][] {
  if (root === null) return [];

  // BFS for each level
  let queue: QueueEntry[] = [{
    node: root,
    depth: 0,
  }];
  let result: number[][] = [];
  while (queue.length > 0) {
    let { node, depth }  = queue.shift()!;
    if (result[depth] === undefined) {
      result[depth] = [];
    }
    result[depth].push(node.val);

    if (node.left !== null) {
      queue.push({
        node: node.left,
        depth: depth + 1,
      });
    }
    if (node.right !== null) {
      queue.push({
        node: node.right,
        depth: depth + 1,
      });
    }
  }
  return result;
}
