use std::collections::HashSet;

use crate::common::*;
use sscanf::sscanf;

enum Dir {
    Up,
    Down,
    Left,
    Right,
}

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

    show(head_x, head_y, tail_x, tail_y);

    raw.lines().for_each(|line| {
        let (d, amount) = sscanf!(line, "{char} {i64}").unwrap();
        for _ in 0..amount {
            match d {
                'R' => head_x += 1,
                'L' => head_x -= 1,
                'U' => head_y -= 1,
                'D' => head_y += 1,
                _   => panic!("Invalid direction!"),
            }

            if i64::abs(head_x - tail_x) > 1 {
                tail_y = head_y;
                if tail_x > head_x {
                    tail_x -= 1;
                } else {
                    tail_x += 1;
                }
            } else if i64::abs(head_y - tail_y) > 1 {
                tail_x = head_x;
                if tail_y > head_x {
                    tail_y -= 1;
                } else {
                    tail_y += 1;
                }
            }
            show(head_x, head_y, tail_x, tail_y);

            visited.insert((tail_x, tail_y));
        }
    });
    Ok(visited.len())
}

fn show(head_x: i64, head_y: i64, tail_x: i64, tail_y: i64) {
    let mut grid = vec![vec![' '; 10]; 10];
    grid[5 + tail_y as usize][5 + tail_x as usize] = 'T';
    grid[5 + head_y as usize][5 + head_x as usize] = 'H';
    println!("------");
    let x = grid.iter().map(|row| row.iter().collect::<String>()).collect::<Vec<_>>();
    println!("{}", x.join("\n"));
    println!("------");
}
