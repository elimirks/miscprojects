use crate::common::*;

pub fn day04() -> AocResult<()> {
    let raw = std::fs::read_to_string("data/day04.txt")?;
    let bounds = raw.lines().map(|line| {
        let (lhs, rhs) = line.split_once(',').expect("No comma found");
        let (lb, le) = to_int_range(lhs);
        let (rb, re) = to_int_range(rhs);
        (lb, le, rb, re)
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

fn to_int_range(s: &str) -> (u8, u8) {
    let (lhs, rhs) = s.split_once('-').expect("No dash found");
    (lhs.parse::<u8>().expect("lhs is not an int"),
     rhs.parse::<u8>().expect("rhs is not an int"))
}
