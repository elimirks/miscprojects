impl Solution {
  pub fn maximum_score_after_operations(
    edges: Vec<Vec<i32>>,
    values: Vec<i32>,
  ) -> i64 {
    let mut edge_map: Vec<Vec<usize>> = vec![vec![]; values.len()];
    for edge in edges {
      edge_map[edge[0] as usize].push(edge[1] as usize);
      edge_map[edge[1] as usize].push(edge[0] as usize);
    }
    prune_parent_edges(&mut edge_map);

    let sums = compute_sums(&edge_map, &values);
    return solve(0, &edge_map, &values, &sums);
  }
}

fn prune_parent_edges(edge_map: &mut [Vec<usize>]) {
  let mut stack = vec![0];
  while let Some(index) = stack.pop() {
    for i in 0..edge_map[index].len() {
      let child = edge_map[index][i];
      let parent_edge_pos = edge_map[child].iter()
        .position(|&i| i == index).unwrap();
      edge_map[child].remove(parent_edge_pos);
      stack.push(child);
    }
  }
}

fn compute_sums(
  edge_map: &[Vec<usize>],
  values: &[i32],
) -> Vec<i64> {
  enum Op {
    Call(usize),
    Aggregate(usize),
  }

  let mut sums = vec![0i64; values.len()];
  let mut stack = vec![Op::Call(0)];
  while let Some(op) = stack.pop() {
    match op {
        Op::Call(index) => {
          stack.push(Op::Aggregate(index));
          for &child in &edge_map[index] {
            stack.push(Op::Call(child));
          }
        }
        Op::Aggregate(index) => {
          let mut sum = values[index] as i64;
          for &child in &edge_map[index] {
            sum += sums[child];
          }
          sums[index] = sum;
        }
    }
  }
  sums
}

fn solve(
  index: usize,
  edge_map: &[Vec<usize>],
  values: &[i32],
  sums: &[i64],
) -> i64 {
  if edge_map[index].len() == 0 {
    return 0;
  }
  // Case 1: Select the current node and recurse for each child
  let mut child_sum = values[index] as i64;
  for &child in &edge_map[index] {
    child_sum += solve(child, edge_map, values, sums);
  }
  // Case 2: Leave the current node and sum the children
  child_sum.max(sums[index] - values[index] as i64)
}
