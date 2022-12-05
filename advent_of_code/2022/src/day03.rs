use crate::common::*;
use std::collections::HashSet;

pub fn day03() -> AocResult<()> {
    let raw = std::fs::read_to_string("data/day03.txt")?;
    let p1_answer = raw.lines().map(|line| {
            let (lhs, rhs) = line.split_at(line.len() / 2);
            find_overlap(lhs.chars().collect::<HashSet<_>>(), rhs)
        })
        .map(priority)
        .sum::<u64>();
    println!("Part 1: {}", p1_answer);

    let mut p2_answer: u64 = 0;
    let mut p2_lines = raw.lines();
    while let (Some(a), Some(b), Some(c)) = (p2_lines.next(), p2_lines.next(), p2_lines.next()) {
        let a_chars = a.chars().collect::<HashSet<_>>();
        let b_chars = b.chars().collect::<HashSet<_>>();
        let intersection = a_chars.intersection(&b_chars)
            .copied()
            .collect::<HashSet<_>>();
        p2_answer += priority(find_overlap(intersection, c));
    }
    println!("Part 2: {}", p2_answer);

    Ok(())
}

fn priority(c: char) -> u64 {
    let n = c as u64;
    if n >= 'a' as u64 && n <= 'z' as u64 {
        n - 'a' as u64 + 1
    } else {
        n - 'A' as u64 + 27
    }
}

fn find_overlap(set: HashSet<char>, s: &str) -> char {
    for c in s.chars() {
        if set.contains(&c) {
            return c;
        }
    }
    panic!("Could not find overlapping char");
}
