use std::collections::BinaryHeap;
use crate::common::*;

pub fn day01() -> AocResult<()> {
    let data = std::fs::read_to_string("data/day1_part1.txt")?;
    let mut calories: BinaryHeap<u64> = data.split("\n\n").map(|chunk| {
        chunk.split("\n")
            .map(|line| line.parse::<u64>().unwrap_or(0))
            .sum()
    }).collect();

    println!("Part 1:");
    println!("{}", calories.peek().unwrap_or(&0));
    println!("Part 2:");
    println!("{}", calories.pop().unwrap_or(0) + calories.pop().unwrap_or(0) + calories.pop().unwrap_or(0));
    Ok(())
}
