use crate::common::*;

#[derive(Clone)]
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

pub fn day13() -> AocResult<()> {
    let pairs = parse_root(std::fs::read_to_string("data/day13.txt")?);
    // for (upper, lower) in pairs.iter() {
    //     println!("============\n{:?}\n{:?}", upper, lower);
    // }
    println!("Part 1: {}", part1(&pairs));
    Ok(())
}

fn part1(pairs: &[(Packet, Packet)]) -> usize {
    pairs.iter().enumerate().filter(|(_, (upper, lower))| {
        is_in_order(upper, lower)
    }).map(|(index, _)| {
        println!("{}", index + 1);
        index + 1
    }).sum::<usize>()
}

fn is_in_order(upper: &Packet, lower: &Packet) -> bool {
    match (upper, lower) {
        (Packet::Value(lhs), Packet::Value(rhs)) => lhs <= rhs,
        (lhs @ Packet::Value(_), rhs) =>
            is_in_order(&Packet::Group(vec![lhs.clone()]), rhs),
        (lhs, rhs @ Packet::Value(_)) =>
            is_in_order(lhs, &Packet::Group(vec![rhs.clone()])),
        (Packet::Group(lhs), Packet::Group(rhs)) => {
            if lhs.is_empty() && rhs.is_empty() {
                true
            } else if lhs.is_empty() {
                true
            } else if rhs.is_empty() {
                false
            } else {
                if !is_in_order(&lhs[0], &rhs[0]) {
                    false
                } else {
                    let new_lhs = lhs.iter().skip(1).cloned().collect::<Vec<_>>();
                    let new_rhs = rhs.iter().skip(1).cloned().collect::<Vec<_>>();
                    is_in_order(&Packet::Group(new_lhs), &Packet::Group(new_rhs))
                }
            }
        },
    }
}

// Recursive descent parser because it's easier to work with
struct ParseContext {
    chars: Vec<char>,
    pos: usize,
}

fn parse_root(s: String) -> Vec<(Packet, Packet)> {
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
            '[' => {
                content.push(parse_group(ctx));
            },
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
