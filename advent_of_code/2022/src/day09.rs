use std::collections::HashSet;

use crate::common::*;
use sscanf::sscanf;

pub fn day09() -> AocResult<()> {
    println!("Part 1: {}", solve::<2>()?);
    println!("Part 2: {}", solve::<10>()?);
    Ok(())
}

fn solve<const N: usize>() -> AocResult<usize> {
    let mut visited = HashSet::new();
    let mut rope = vec![(0, 0); N];
    std::fs::read_to_string("data/day09.txt")?.lines().for_each(|line| {
        let (dir, amount) = sscanf!(line, "{char} {i64}").unwrap();
        for _ in 0..amount {
            match dir {
                'L' => rope[0].0 -= 1,
                'R' => rope[0].0 += 1,
                'U' => rope[0].1 -= 1,
                'D' => rope[0].1 += 1,
                _   => panic!("Invalid direction!"),
            }
            propagate_rope(&mut rope);
            visited.insert(rope.last().unwrap().clone());
        }
    });
    Ok(visited.len())
}

fn propagate_rope(rope: &mut Vec<(i64, i64)>) {
    for i in 1..rope.len() {
        let (hx, hy) = rope[i - 1];
        let (tx, ty) = rope[i];
        let (dx, dy) = (hx - tx, hy - ty);
        if dx.abs() > 1 || dy.abs() > 1 {
            rope[i].0 += dx.signum();
            rope[i].1 += dy.signum();
        }
    }
}
