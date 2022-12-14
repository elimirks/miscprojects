use crate::common::*;
use sscanf::sscanf;

pub fn day10() -> AocResult<()> {
    let reg_values = parse_reg_values()?;
    println!("Part 1: {}", part1(&reg_values));
    println!("Part 2:");
    part2(&reg_values);
    Ok(())
}

fn part1(reg_values: &[i64]) -> i64 {
    reg_values.iter().enumerate().skip(19).step_by(40).map(|(index, &xvalue)| {
        (index as i64 + 1) * xvalue
    }).sum()
}

fn part2(reg_values: &[i64]) {
    for y in 0..6 {
        for x in 0..40 {
            if (reg_values[x + y * 40] - x as i64).abs() < 2 {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
}

fn parse_reg_values() -> AocResult<Vec<i64>> {
    let mut reg_values = vec![1];
    std::fs::read_to_string("data/day10.txt")?.lines().for_each(|line| {
        let previous = *reg_values.last().unwrap();
        reg_values.push(previous);
        if let Ok(value) = sscanf!(line, "addx {i64}") {
            reg_values.push(value + previous);
        }
    });
    Ok(reg_values)
}
