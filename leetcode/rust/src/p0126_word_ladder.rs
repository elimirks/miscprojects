use std::collections::VecDeque;

type Index = u16; // Max of 500 words, so u16 is good enough

impl Solution {
  pub fn find_ladders(
    begin_word: String,
    end_word: String,
    mut word_list: Vec<String>,
  ) -> Vec<Vec<String>> {
    if begin_word == end_word {
      return vec![vec![begin_word]];
    }

    if !word_list.contains(&begin_word) {
      word_list.push(begin_word.clone());
    }

    // Step 1: Build edge-list graph connecting adjacent words
    let mut edges: Vec::<Vec<Index>> = vec![vec![]; word_list.len()];

    for (i, first_word) in word_list.iter().enumerate() {
      for (j, second_word) in word_list.iter().enumerate().skip(i + 1) {
        let mut diff_byte_count = 0;
        for (a, b) in first_word.bytes().zip(second_word.bytes()) {
          if a != b {
            diff_byte_count += 1;
          }
        }

        if diff_byte_count == 1 {
          edges[i].push(j as Index);
          edges[j].push(i as Index);
        }
      }
    }

    // Step 2: BFS to find the shortest distance
    let begin_index = word_list.iter().position(|word| word == &begin_word)
      .expect("begin_word not in word_list") as Index;
    let Some(end_index) = word_list.iter().position(|word| word == &end_word) else {
      return vec![];
    };
    let end_index = end_index as Index;

    let mut queue = VecDeque::<Index>::new();
    queue.push_front(begin_index);

    // Stores the depth of each visited node.
    // So we can tell if we should bless them or not
    let mut visited = vec![u16::MAX; word_list.len()];
    visited[begin_index as usize] = 0;

    // Edges "blessed" during BFS
    // We don't want to traverse any other paths during the DFS stage
    // This graph is guaranteed to have no cycles
    let mut blessed_edges: Vec::<Vec<Index>> = vec![vec![]; word_list.len()];

    while let Some(cur_index) = queue.pop_back() {
      if cur_index == end_index {
        break;
      }
      let depth = visited[cur_index as usize];
      for next in &edges[cur_index as usize] {
        let visited_depth = visited[*next as usize];

        if visited_depth == depth + 1 || visited_depth == u16::MAX {
          let blessed = &mut blessed_edges[cur_index as usize];
          blessed.push(*next);
        }
        if visited_depth == u16::MAX {
          visited[*next as usize] = depth + 1;
          queue.push_front(*next);
        }
      }
    }

    // Step 3: DFS to collect the results 
    dfs(begin_index, end_index, &blessed_edges, &word_list)
  }
}

enum DfsOp {
  Call(Index),
  PushPath(Index),
  PopPath,
}

fn dfs(
  begin_index: Index,
  end_index: Index,
  edges: &Vec<Vec<Index>>,
  word_list: &[String],
) -> Vec<Vec<String>> {
  use DfsOp::*;
  let mut result = vec![];
  let mut path = vec![begin_index];
  let mut stack = vec![Call(begin_index)];

  while let Some(op) = stack.pop() {
    match op {
        Call(cur_index) => {
          if cur_index == end_index {
            let str_path = path.iter()
              .map(|index| word_list[*index as usize].clone())
              .collect::<Vec<_>>();
            result.push(str_path);
          } else {
            for next in &edges[cur_index as usize] {
              stack.push(PopPath);
              stack.push(Call(*next));
              stack.push(PushPath(*next));
            }
          }
        }
        PushPath(index) => {
          path.push(index);
        }
        PopPath => {
          path.pop();
        }
    }
  }
  result
}
