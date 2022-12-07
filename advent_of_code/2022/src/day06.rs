use crate::common::*;
use std::collections::HashSet;

pub fn day06() -> AocResult<()> {
    let raw = std::fs::read_to_string("data/day06.txt")?;
    println!("Part 1: {}", solve::<4>(&raw));
    println!("Part 2: {}", solve::<14>(&raw));
    Ok(())
}

fn solve<const N: usize>(input: &str) -> usize {
    let mut buf = [' '; N];
    for (i, c) in input.chars().enumerate() {
        buf[i % N] = c;
        if i >= N && buf.iter().collect::<HashSet<_>>().len() == N {
            return i + 1;
        }
    }
    panic!("No solution for input");
}
