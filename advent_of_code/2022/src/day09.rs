use std::collections::HashSet;

use crate::common::*;
use sscanf::sscanf;

pub fn day09() -> AocResult<()> {
    println!("Part 1: {}", part1()?);
    Ok(())
}

pub fn part1() -> AocResult<usize> {
    let raw = std::fs::read_to_string("data/day09.txt")?;
    let mut visited = HashSet::new();
    let mut head_x: i64 = 0;
    let mut head_y: i64 = 0;
    let mut tail_x: i64 = 0;
    let mut tail_y: i64 = 0;
    raw.lines().for_each(|line| {
        let (dir, amount) = sscanf!(line, "{char} {i64}").unwrap();
        for _ in 0..amount {
            let (dx, dy) = match dir {
                'R' => (1, 0),
                'L' => (-1, 0),
                'U' => (0, -1),
                'D' => (0, 1),
                _   => panic!("Invalid direction!"),
            };
            head_x += dx;
            head_y += dy;
            if i64::abs(head_x - tail_x).max(i64::abs(head_y - tail_y)) > 1 {
                tail_y = head_y - dy;
                tail_x = head_x - dx;
            }
            visited.insert((tail_x, tail_y));
        }
    });
    Ok(visited.len())
}
