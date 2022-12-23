use std::collections::{HashSet, HashMap};

use itertools::Itertools;

use crate::common::*;

#[derive(Debug, Copy, Clone)]
enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    fn next(&self) -> Direction {
        match self {
            Direction::North => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::East,
            Direction::East => Direction::North,
        }
    }
}

pub fn day23() -> AocResult<()> {
    let elves = parse(&std::fs::read_to_string("data/day23.txt")?);
    println!("Part 1: {}", part1(elves.clone()));
    println!("Part 2: {}", part2(elves));
    Ok(())
}

fn part1(mut elves: Vec<(i64, i64)>) -> i64 {
    let mut direction = Direction::North;
    for _ in 0..10 {
        tick(&mut elves, direction);
        direction = direction.next();
    }
    let (min_x, max_x) = elves.iter().map(|p| p.0).minmax().into_option().unwrap();
    let (min_y, max_y) = elves.iter().map(|p| p.1).minmax().into_option().unwrap();
    let area = (max_x - min_x + 1) * (max_y - min_y + 1);
    area - elves.len() as i64
}

fn part2(mut elves: Vec<(i64, i64)>) -> i64 {
    let mut direction = Direction::North;
    let mut round = 1;
    while tick(&mut elves, direction) {
        direction = direction.next();
        round += 1;
    }
    round
}

// Returns true when all the elves are done moving
fn tick(
    elves: &mut Vec<(i64, i64)>,
    direction: Direction,
) -> bool {
    let occupied = elves.iter().cloned().collect::<HashSet<_>>();
    let mut proposals = elves.iter()
        .map(|elf| propose_move(&occupied, elf, direction))
        .collect::<Vec<_>>();
    retain_unique_proposals(&mut proposals);
    // No proposals about where to move... we're done!
    if proposals.iter().all(|value| value.is_none()) {
        return false;
    }
    for i in 0..elves.len() {
        if let Some(new_point) = proposals[i] {
            elves[i] = new_point;
        }
    }
    true
}

fn retain_unique_proposals(proposals: &mut [Option<(i64, i64)>]) {
    let mut proposal_counts = HashMap::new();
    for point in proposals.iter().flatten() {
        let new_count = proposal_counts.get(point).unwrap_or(&0) + 1;
        proposal_counts.insert(point, new_count);
    }
    let unique_points = proposal_counts.iter().filter_map(|(point, count)| {
        if *count == 1 {
            Some(**point)
        } else {
            None
        }
    }).collect::<HashSet<_>>();

    for proposal in proposals.iter_mut() {
        if let Some(point) = proposal {
            if !unique_points.contains(point) {
                *proposal = None;
            }
        }
    }
}

fn propose_move(
    occupied: &HashSet<(i64, i64)>,
    elf: &(i64, i64),
    mut direction: Direction,
) -> Option<(i64, i64)> {
    let has_neighbor = (-1..=1).cartesian_product(-1..=1).any(|(x, y)| {
        (x != 0 || y != 0) && occupied.contains(&(elf.0 + x, elf.1 + y))
    });
    if !has_neighbor {
        return None;
    }
    for _ in 0..4 {
        let coords_to_check = match direction {
            Direction::North => &[(-1, -1), (0, -1), (1, -1)],
            Direction::South => &[(-1, 1), (0, 1), (1, 1)],
            Direction::West => &[(-1, -1), (-1, 0), (-1, 1)],
            Direction::East => &[(1, -1), (1, 0), (1, 1)],
        };
        if coords_to_check.iter().all(|coord| {
            let x = coord.0 + elf.0;
            let y = coord.1 + elf.1;
            !occupied.contains(&(x, y))
        }) {
            return Some(match direction {
                Direction::North => (elf.0, elf.1 - 1),
                Direction::South => (elf.0, elf.1 + 1),
                Direction::West => (elf.0 - 1, elf.1),
                Direction::East => (elf.0 + 1, elf.1),
            });
        }
        direction = direction.next();
    }
    None
}

// Returns the coordinates of all elves
fn parse(data: &str) -> Vec<(i64, i64)> {
    data.lines().enumerate().flat_map(|(y, line)| {
        line.chars().enumerate().filter_map(move |(x, c)| {
            if c == '#' {
                Some((x as i64, y as i64))
            } else {
                None
            }
        })
    }).collect::<Vec<_>>()
}
