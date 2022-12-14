use std::cmp::Ordering;

use crate::common::*;

#[derive(Clone, Eq)]
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
                if lhs.len() == rhs.len() {
                    Some(Ordering::Equal)
                } else {
                    Some(Ordering::Less) // LHS ran out of values first
                }
            },
        }
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl PartialEq for Packet {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

pub fn day13() -> AocResult<()> {
    let pairs = parse_root(&std::fs::read_to_string("data/day13.txt")?);
    println!("Part 1: {}", part1(&pairs));
    println!("Part 2: {}", part2(&pairs));
    Ok(())
}

fn part1(pairs: &[(Packet, Packet)]) -> usize {
    pairs.iter().enumerate()
        .filter(|(_, (upper, lower))| upper <= lower)
        .map(|(index, _)| index + 1)
        .sum::<usize>()
}

fn part2(pairs: &[(Packet, Packet)]) -> usize {
    let mut all_packets = pairs.iter()
        .flat_map(|(upper, lower)| vec![upper, lower])
        .collect::<Vec<_>>();
    let div_packet_2 = create_divider_packet(2);
    let div_packet_6 = create_divider_packet(6);
    all_packets.push(&div_packet_2);
    all_packets.push(&div_packet_6);
    all_packets.sort();

    let pos_2 = all_packets.iter().position(|value| *value == &div_packet_2).unwrap();
    let pos_6 = all_packets.iter().position(|value| *value == &div_packet_6).unwrap();
    (pos_2 + 1) * (pos_6 + 1)
}

fn create_divider_packet(value: i64) -> Packet {
    Packet::Group(vec![Packet::Group(vec![Packet::Value(value)])])
}

// Recursive descent parser because it's easier to work with
struct ParseContext {
    chars: Vec<char>,
    pos: usize,
}

fn parse_root(s: &str) -> Vec<(Packet, Packet)> {
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
    use more_asserts::*;
    use super::*;

    #[test]
    fn test_parsing() {
        let content = std::fs::read_to_string("data/day13.txt").unwrap();
        let pairs = parse_root(&content);
        let parsed = pairs.iter().map(|(upper, lower)| {
            format!("{upper:?}\n{lower:?}\n\n")
        }).collect::<String>();
        assert_eq!(content, parsed);
    }

    fn parse_packet(s: &str) -> Packet {
        let mut ctx = ParseContext {
            chars: s.chars().collect::<Vec<_>>(),
            pos: 0,
        };
        parse_group(&mut ctx)
    }

    #[test]
    fn test_comparison_example_case() {
        assert_lt!(
            parse_packet("[1,1,3,1,1]"),
            parse_packet("[1,1,5,1,1]")
        );
        assert_lt!(
            parse_packet("[[1],[2,3,4]]"),
            parse_packet("[[1],4]")
        );
        assert_gt!(
            parse_packet("[9]"),
            parse_packet("[[8,7,6]]")
        );
        assert_lt!(
            parse_packet("[[4,4],4,4]"),
            parse_packet("[[4,4],4,4,4]")
        );
        assert_gt!(
            parse_packet("[7,7,7,7]"),
            parse_packet("[7,7,7]")
        );
        assert_lt!(
            parse_packet("[]"),
            parse_packet("[3]")
        );
        assert_gt!(
            parse_packet("[[[]]]"),
            parse_packet("[[]]")
        );
        assert_gt!(
            parse_packet("[1,[2,[3,[4,[5,6,7]]]],8,9]"),
            parse_packet("[1,[2,[3,[4,[5,6,0]]]],8,9]")
        );
    }

    #[test]
    fn test_comparison_equality() {
        assert_eq!(
            parse_packet("[1]"),
            parse_packet("[[1]]")
        );
        assert_eq!(
            parse_packet("[[[[[1]]]]]"),
            parse_packet("[[1]]")
        );
        assert_eq!(
            parse_packet("[[[2]],3,4]"),
            parse_packet("[2,3,[4]]")
        );
    }
}
