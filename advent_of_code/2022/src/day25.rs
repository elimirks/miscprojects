use std::fmt::{Debug, Write};

use crate::common::*;

#[derive(PartialEq, Eq)]
struct Snafu {
    digits: Vec<i64>,
}

impl Debug for Snafu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for d in self.digits.iter() {
            f.write_char(match d {
                0 => '0',
                1 => '1',
                2 => '2',
                -1 => '-',
                -2 => '=',
                _ => unreachable!(),
            })?;
        }
        Ok(())
    }
}

fn to_dec(snafu: &Snafu) -> i64 {
    snafu.digits.iter().fold(0, |acc, it| acc * 5 + it)
}

fn to_snafu(mut num: i64) -> Snafu {
    let mut digits = vec![];
    while num != 0 {
        let rem = num % 5;
        digits.push((rem + 2) % 5 - 2);
        num /= 5;
        if rem > 2 {
            num += 1;
        }
    }
    digits.reverse();
    Snafu { digits }
}

pub fn day25() -> AocResult<()> {
    let nums = parse(&std::fs::read_to_string("data/day25.txt")?);
    let target = nums.iter().map(to_dec).sum();
    println!("Answer: {:?}", to_snafu(target));
    Ok(())
}

fn parse(s: &str) -> Vec<Snafu> {
    s.lines().map(|line| {
        let digits = line.chars().map(|c| match c {
            '0' => 0,
            '1' => 1,
            '2' => 2,
            '-' => -1,
            '=' => -2,
            c   => panic!("Unexpected char: {c}"),
        }).collect::<Vec<_>>();
        Snafu { digits }
    }).collect::<Vec<_>>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_dec() {
        assert_eq!(3, to_dec(&Snafu { digits: vec![1, -2] }));
        assert_eq!(1747, to_dec(&Snafu { digits: vec![1, -2, -1, 0, -1, 2] }));
    }

    #[test]
    fn test_to_snafu() {
        assert_eq!(Snafu { digits: vec![1, -2] }, to_snafu(3));
        assert_eq!(Snafu { digits: vec![1, -2, -1, 0, -1, 2] }, to_snafu(1747));
    }
}
