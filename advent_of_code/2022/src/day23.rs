use std::collections::{HashSet, HashMap};
use itertools::Itertools;

use crate::common::*;

#[derive(Copy, Clone)]
enum Dir {
    N,
    S,
    W,
    E,
}

impl Dir {
    fn next(&self) -> Dir {
        match self {
            Dir::N => Dir::S,
            Dir::S => Dir::W,
            Dir::W => Dir::E,
            Dir::E => Dir::N,
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
    let mut dir = Dir::N;
    for _ in 0..10 {
        tick(&mut elves, dir);
        dir = dir.next();
    }
    let (min_x, max_x) = elves.iter().map(|p| p.0).minmax().into_option().unwrap();
    let (min_y, max_y) = elves.iter().map(|p| p.1).minmax().into_option().unwrap();
    let area = (max_x - min_x + 1) * (max_y - min_y + 1);
    area - elves.len() as i64
}

fn part2(mut elves: Vec<(i64, i64)>) -> i64 {
    let mut dir = Dir::N;
    let mut round = 1;
    while tick(&mut elves, dir) {
        dir = dir.next();
        round += 1;
    }
    round
}

// Returns true when all the elves are done moving
fn tick(
    elves: &mut Vec<(i64, i64)>,
    dir: Dir,
) -> bool {
    let occupied = elves.iter().cloned().collect::<HashSet<_>>();
    let mut proposals = elves.iter()
        .map(|elf| propose_move(&occupied, elf, dir))
        .collect::<Vec<_>>();
    retain_unique_proposals(&mut proposals);
    let mut has_changed = false;
    for i in 0..elves.len() {
        if let Some(new_point) = proposals[i] {
            elves[i] = new_point;
            has_changed = true;
        }
    }
    has_changed
}

fn retain_unique_proposals(proposals: &mut [Option<(i64, i64)>]) {
    let mut proposal_counts = HashMap::new();
    for point in proposals.iter().flatten() {
        let new_count = proposal_counts.get(point).unwrap_or(&0) + 1;
        proposal_counts.insert(point, new_count);
    }
    let unique_points = proposal_counts.iter().filter_map(|(&&point, &count)| {
        if count == 1 {
            Some(point)
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
    mut dir: Dir,
) -> Option<(i64, i64)> {
    let has_neighbor = (-1..=1).cartesian_product(-1..=1).any(|(x, y)| {
        (x != 0 || y != 0) && occupied.contains(&(elf.0 + x, elf.1 + y))
    });
    if !has_neighbor {
        return None;
    }
    for _ in 0..4 {
        let coords_to_check = match dir {
            Dir::N => &[(-1, -1), (0, -1), (1, -1)],
            Dir::S => &[(-1, 1), (0, 1), (1, 1)],
            Dir::W => &[(-1, -1), (-1, 0), (-1, 1)],
            Dir::E => &[(1, -1), (1, 0), (1, 1)],
        };
        if coords_to_check.iter().all(|coord| {
            let x = coord.0 + elf.0;
            let y = coord.1 + elf.1;
            !occupied.contains(&(x, y))
        }) {
            return Some(match dir {
                Dir::N => (elf.0, elf.1 - 1),
                Dir::S => (elf.0, elf.1 + 1),
                Dir::W => (elf.0 - 1, elf.1),
                Dir::E => (elf.0 + 1, elf.1),
            });
        }
        dir = dir.next();
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
