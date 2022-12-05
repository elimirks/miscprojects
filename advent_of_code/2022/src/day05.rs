use sscanf::sscanf;
use crate::common::*;

pub fn day05() -> AocResult<()> {
    let stacks = parse_header()?;
    let moves = parse_moves()?;
    println!("Part 1: {}", part1(stacks.clone(), &moves));
    println!("Part 2: {}", part2(stacks, &moves));
    Ok(())
}

fn part1(mut stacks: Vec<Vec<char>>, moves: &[(u8, usize, usize)]) -> String {
    for &(amount, from_stack, to_stack) in moves.iter() {
        for _ in 0..amount {
            let c = stacks[from_stack].pop().expect("Popped off empty stack");
            stacks[to_stack].push(c);
        }
    }
    stack_tops(&stacks)
}

fn part2(mut stacks: Vec<Vec<char>>, moves: &[(u8, usize, usize)]) -> String {
    for &(amount, from_stack, to_stack) in moves.iter() {
        let split_idx = stacks[from_stack].len() - amount as usize;
        let mut popped = stacks[from_stack].split_off(split_idx);
        stacks[to_stack].append(&mut popped);
    }
    stack_tops(&stacks)
}

fn stack_tops(stacks: &[Vec<char>]) -> String {
    stacks.iter().flat_map(|s| s.last()).collect()
}

fn parse_header() -> AocResult<Vec<Vec<char>>> {
    let raw = std::fs::read_to_string("data/day05.txt")?;
    let header = raw.lines().take_while(|line| !line.starts_with(' ')).collect::<Vec<_>>();
    let mut stacks: Vec<Vec<char>> = Vec::new();
    for _ in 0..9 {
        stacks.push(Vec::new());
    }
    for line in header.iter().rev() {
        line.chars().skip(1).step_by(4).enumerate()
            .filter(|&(_, c)| c != ' ')
            .for_each(|(i, c)| stacks[i].push(c));
    }
    Ok(stacks)
}

/// Returns moves in form of (amount, from_stack, to_stack)
fn parse_moves() -> AocResult<Vec<(u8, usize, usize)>> {
    let raw = std::fs::read_to_string("data/day05.txt")?;
    Ok(raw.lines().skip_while(|line| !line.starts_with("move"))
        .map(|line| {
            let result = sscanf!(line, "move {u8} from {usize} to {usize}")
                .expect("Invalid move line");
            (result.0, result.1 - 1, result.2 - 1)
        })
        .collect()
    )
}
