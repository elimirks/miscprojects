use sscanf::sscanf;
use crate::common::*;

pub fn day04() -> AocResult<()> {
    let raw = std::fs::read_to_string("data/day04.txt")?;
    let bounds = raw.lines().map(|line| {
        sscanf!(line, "{u8}-{u8},{u8}-{u8}").expect("Invalid formatting")
    }).collect::<Vec<_>>();

    let p1_answer = bounds.iter().filter(|(lb, le, rb, re)| {
        (lb >= rb && le <= re) || (rb >= lb && re <= le)
    }).count();
    println!("Part 1: {}", p1_answer);

    let p2_answer = bounds.iter().filter(|(lb, le, rb, re)| {
        (lb >= rb && lb <= re) || (le >= rb && le <= re) || (lb < rb && le > re)
    }).count();
    println!("Part 2: {}", p2_answer);

    Ok(())
}
