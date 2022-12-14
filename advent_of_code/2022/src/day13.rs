use std::cmp::Ordering;

use crate::common::*;

#[derive(Clone, PartialEq, Eq)]
enum Packet {
    Value(i64),
    Group(Vec<Packet>),
}

impl std::fmt::Debug for Packet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Packet::Value(value) => write!(f, "{value}"),
            Packet::Group(values) => {
                write!(f, "[")?;
                let l = values.len();
                for (i, value) in values.iter().enumerate() {
                    write!(f, "{:?}", value)?;
                    if i != l - 1 {
                        write!(f, ",")?;
                    }
                }
                write!(f, "]")
            },
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Packet::Value(lhs), Packet::Value(rhs)) => lhs.partial_cmp(rhs),
            (lhs @ Packet::Value(_), rhs @ Packet::Group(_)) =>
                Packet::Group(vec![lhs.clone()]).partial_cmp(rhs),
            (lhs @ Packet::Group(_), rhs @ Packet::Value(_)) => 
                lhs.partial_cmp(&Packet::Group(vec![rhs.clone()])),
            (Packet::Group(lhs), Packet::Group(rhs)) => {
                for i in 0..lhs.len() {
                    if i >= rhs.len() {
                        return Some(Ordering::Greater);
                    }
                    let sub = lhs[i].partial_cmp(&rhs[i]);
                    if sub != Some(Ordering::Equal) {
                        return sub;
                    }
                }
                Some(Ordering::Less) // LHS ran out of values first
            },
        }
    }
}

pub fn day13() -> AocResult<()> {
    let pairs = parse_root(&std::fs::read_to_string("data/day13.txt")?);
    //println!("Part 1: {}", part1(&pairs));
    Ok(())
}

fn part1(pairs: &[(Packet, Packet)]) -> usize {
    pairs.iter().enumerate()
        .filter(|(_, (upper, lower))| upper <= lower)
        .map(|(index, _)| index + 1)
        .sum::<usize>()
}

// Recursive descent parser because it's easier to work with
struct ParseContext {
    chars: Vec<char>,
    pos: usize,
}

fn parse_root(s: &String) -> Vec<(Packet, Packet)> {
    let mut pairs = vec![];
    let mut ctx = ParseContext {
        chars: s.chars().collect::<Vec<_>>(),
        pos: 0,
    };
    while ctx.pos < ctx.chars.len() {
        let upper = parse_group(&mut ctx);
        ctx.pos += 1; // Skip newline between pair elements
        let lower = parse_group(&mut ctx);
        ctx.pos += 2; // Skip double newline between pairs
        pairs.push((upper, lower));
    }
    pairs
}

// Expects `[` to NOT have been seeked over
fn parse_group(ctx: &mut ParseContext) -> Packet {
    ctx.pos += 1;
    let mut content = vec![];
    while let Some(c) = ctx.chars.get(ctx.pos) {
        match c {
            ']' => {
                ctx.pos += 1;
                return Packet::Group(content)
            },
            '[' => content.push(parse_group(ctx)),
            ',' => ctx.pos += 1,
            // Otherwise, assume it's a digit
            _ => content.push(parse_int(ctx)),
        }
    }
    panic!("Didn't find closing ]")
}

fn parse_int(ctx: &mut ParseContext) -> Packet {
    let mut value = 0;
    while let Some(c) = ctx.chars.get(ctx.pos) {
        if let Some(d) = c.to_digit(10) {
            value = 10 * value + d as i64;
            ctx.pos += 1;
        } else {
            break;
        }
    }
    Packet::Value(value)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parsing() {
        let content = std::fs::read_to_string("data/day13.txt").unwrap();
        let pairs = parse_root(&content);

        let parsed = pairs.iter().map(|(upper, lower)| {
        });
        for (upper, lower) in pairs.iter() {
            parsed.push_str(&format!("{upper:?}\n"));
            parsed.push_str(&format!("{lower:?}\n\n"));
        }
        assert_eq!(content, parsed);
    }
}
