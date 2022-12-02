use crate::common::*;

#[derive(PartialEq, Clone, Copy)]
enum Move {
    Rock,
    Paper,
    Scissors,
}

enum Outcome {
    Lose,
    Draw,
    Win,
}

pub fn day02() -> AocResult<()> {
    let data = std::fs::read_to_string("data/day2_part1.txt")?;
    let part1_pairs = data.split('\n').filter(|line| !line.is_empty()).map(|line| {
        let lhs = match line.chars().next() {
            Some('A') => Move::Rock,
            Some('B') => Move::Paper,
            Some('C') => Move::Scissors,
            c   => panic!("Invalid LHS: {:?}", c),
        };
        let rhs = match line.chars().nth(2) {
            Some('X') => Move::Rock,
            Some('Y') => Move::Paper,
            Some('Z') => Move::Scissors,
            c   => panic!("Invalid RHS: {:?}", c),
        };
        (lhs, rhs)
    }).collect::<Vec<_>>();
    println!("Part 1: {}", compute_score(&part1_pairs));

    let part2_pairs = part1_pairs.iter().map(|(lhs, rhs)| {
        (lhs, match rhs {
            Move::Rock     => Outcome::Lose,
            Move::Paper    => Outcome::Draw,
            Move::Scissors => Outcome::Win,
        })
    });

    let part2_moves = part2_pairs.map(|(lhs, outcome)| {
        (*lhs, match (lhs, outcome) {
            (Move::Rock, Outcome::Lose)     => Move::Scissors,
            (Move::Rock, Outcome::Win)      => Move::Paper,
            (Move::Paper, Outcome::Lose)    => Move::Rock,
            (Move::Paper, Outcome::Win)     => Move::Scissors,
            (Move::Scissors, Outcome::Lose) => Move::Paper,
            (Move::Scissors, Outcome::Win)  => Move::Rock,
            (lhs, Outcome::Draw) => *lhs,
        })
    }).collect::<Vec<_>>();
    println!("Part 2: {}", compute_score(&part2_moves));
    Ok(())
}

fn compute_score(pairs: &[(Move, Move)]) -> u64 {
    pairs.iter().map(|(lhs, rhs)| {
        let move_score = match rhs {
            Move::Rock     => 1,
            Move::Paper    => 2,
            Move::Scissors => 3,
        };
        move_score + match (lhs, rhs) {
            (lhs, rhs) if lhs == rhs      => 3,
            (Move::Rock, Move::Paper)     => 6,
            (Move::Paper, Move::Scissors) => 6,
            (Move::Scissors, Move::Rock)  => 6,
            _                             => 0
        }
    }).sum()
}
