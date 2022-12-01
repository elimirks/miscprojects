use std::collections::BinaryHeap;
use crate::common::*;

// Returns the amounts of calories each elf is holding
fn parse(lines: Vec<&str>) -> Vec<u64> {
    let mut calories = vec![];
    let mut current_net = 0;
    for line in lines {
        if line.is_empty() {
            calories.push(current_net);
            current_net = 0;
        } else {
            current_net += line.parse::<u64>().unwrap();
        }
    }
    calories
}

pub fn part1(calorie_counts: &BinaryHeap<u64>) {
    println!("Part 1:");
    println!("{}", calorie_counts.peek().unwrap_or(&0));
}

pub fn part2(calorie_counts: &mut BinaryHeap<u64>) {
    println!("Part 2:");
    let sum = calorie_counts.pop().unwrap_or(0)
        + calorie_counts.pop().unwrap_or(0)
        + calorie_counts.pop().unwrap_or(0);
    println!("{}", sum);
}

pub fn day01() -> AocResult<()> {
    let data = std::fs::read_to_string("data/day1_part1.txt")?;
    let lines = data.split("\n");
    let calorie_counts = parse(lines.collect());
    let mut heap = BinaryHeap::from(calorie_counts);

    part1(&heap);
    part2(&mut heap);
    Ok(())
}
