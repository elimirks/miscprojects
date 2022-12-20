use std::collections::VecDeque;

use crate::common::*;

const CHAMBER_WIDTH: usize = 7;
type Chamber = VecDeque<[bool; CHAMBER_WIDTH]>;

#[derive(Debug, Clone)]
enum Movement {
    Left,
    Right,
}

struct Rock {
    points: Vec<(usize, usize)>,
    width: usize,
}

pub fn day17() -> AocResult<()> {
    let data = parse(&std::fs::read_to_string("data/day17.txt")?);
    println!("Part 1: {}", part1(&data));
    println!("Part 2: {}", part2(&data));
    Ok(())
}

fn parse(content: &str) -> Vec<Movement> {
    content.chars()
        .filter_map(|c| match c {
            '>' => Some(Movement::Right),
            '<' => Some(Movement::Left),
            _   => None
        })
        .collect::<Vec<_>>()
}

fn create_rocks() -> Vec<Rock> {
    vec![
        Rock {
            points: vec![(0, 0), (1, 0), (2, 0), (3, 0)],
            width: 4,
        },
        Rock {
            points: vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
            width: 3,
        },
        Rock {
            // "upside down" since we grow the chamber upwards
            points: vec![(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
            width: 3,
        },
        Rock {
            points: vec![(0, 0), (0, 1), (0, 2), (0, 3)],
            width: 1,
        },
        Rock {
            points: vec![(0, 0), (1, 0), (0, 1), (1, 1)],
            width: 2,
        },
    ]
}

fn is_at_rest(
    chamber: &Chamber,
    rock: &Rock,
    rock_x: usize,
    rock_y: usize,
) -> bool {
    rock.points.iter().map(|&(x, y)| {
        (x + rock_x, y + rock_y)
    }).any(|(x, y)| {
        if y > 0 && y < 1 + chamber.len() {
            chamber[y - 1][x]
        } else {
            y == 0
        }
    })
}

// Assumes rock_x is in bounds for the given rock
fn rock_collides(
    chamber: &Chamber,
    rock: &Rock,
    rock_x: usize,
    rock_y: usize,
) -> bool {
    rock.points.iter().map(|&(x, y)| {
        (x + rock_x, y + rock_y)
    }).any(|(x, y)| {
        y < chamber.len() && chamber[y][x]
    })
}

fn clamp_x_movement(
    rock: &Rock,
    movement: &Movement,
    rock_x: usize,
) -> usize {
    match movement {
        Movement::Left => if rock_x > 0 {
            rock_x - 1
        } else {
            0
        },
        Movement::Right => if rock_x + rock.width >= CHAMBER_WIDTH {
            rock_x
        } else {
            rock_x + 1
        },
    }
}

fn add_rock_to_chamber(
    chamber: &mut Chamber,
    rock: &Rock,
    rock_x: usize,
    rock_y: usize
) {
    for &(x, y) in rock.points.iter() {
        let xp = x + rock_x;
        let yp = y + rock_y;
        while yp >= chamber.len() {
            chamber.push_back([false; CHAMBER_WIDTH]);
        }
        chamber[yp][xp] = true;
    }
}

fn is_prunable(chamber: &Chamber, x: usize, y: usize) -> bool {
    if y < CHAMBER_WIDTH {
        false
    } else if x >= CHAMBER_WIDTH {
        true
    } else if !chamber[y][x] {
        false
    } else {
        is_prunable(chamber, x + 1, y) || 
            is_prunable(chamber, x + 1, y + 1) || 
            is_prunable(chamber, x + 1, y - 1)
    }
}

// Returns the number of rows pruned
fn prune_chamber(chamber: &mut Chamber) -> usize {
    if chamber.len() < CHAMBER_WIDTH {
        return 0;
    }
    for y in (CHAMBER_WIDTH..chamber.len() - CHAMBER_WIDTH).rev() {
        if is_prunable(chamber, 0, y) {
            let prune_index = y - CHAMBER_WIDTH;
            chamber.drain(..prune_index);
            return prune_index;
        }
    }
    0
}

fn solve(movements: &[Movement], rock_count: usize) -> usize {
    let mut move_index = 0;
    let rocks = create_rocks();
    // Lower indices are closer to the bottom
    let mut chamber = VecDeque::<[bool; CHAMBER_WIDTH]>::new();
    let mut amount_pruned = 0;
    for i in 0..rock_count {
        let rock = &rocks[i % rocks.len()];
        let mut rock_x = 2;

        // Optimized first 3 moves, since we know it can't hit the floor or other rocks
        for _ in 0..3 {
            rock_x = clamp_x_movement(rock, &movements[move_index], rock_x);
            move_index = (move_index + 1) % movements.len();
        }
        let mut rock_y = chamber.len();

        loop {
            let proposed_x = clamp_x_movement(rock, &movements[move_index], rock_x);
            if !rock_collides(&chamber, rock, proposed_x, rock_y) {
                rock_x = proposed_x;
            }
            move_index = (move_index + 1) % movements.len();
            if is_at_rest(&chamber, rock, rock_x, rock_y) {
                break;
            } else {
                rock_y -= 1;
            }
        }
        add_rock_to_chamber(&mut chamber, rock, rock_x, rock_y);

        if i % 10000 == 0 {
            amount_pruned += prune_chamber(&mut chamber);
        }
        if i % 10000000 == 0 {
            println!("{:.4}%", 100.0 * i as f64 / rock_count as f64);
        }
    }
    amount_pruned + chamber.len()
}

fn part1(movements: &[Movement]) -> usize {
    solve(movements, 2022)
}

fn part2(movements: &[Movement]) -> usize {
    //solve(movements, 10000000 * 10)
    solve(movements, 1000000000000)
}
