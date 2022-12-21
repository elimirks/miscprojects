use crate::common::*;

const CHAMBER_WIDTH: u8 = 7;
type Chamber = Vec<u8>;

#[derive(Debug, Copy, Clone)]
enum Movement {
    Left,
    Right,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Rock {
    points: Vec<u8>,
    width: usize,
    height: usize,
}

impl Rock {
    fn new(points: Vec<u8>) -> Rock {
        let width = points.iter().map(|row| row.count_ones()).max().unwrap() as usize;
        Rock {
            width,
            height: points.len(),
            points,
        }
    }
}

pub fn day17() -> AocResult<()> {
    let data = parse(&std::fs::read_to_string("data/day17.txt")?);
    println!("Part 1: {}", part1(&data));
    println!("Part 2: {}", time_closure(|| part2(&data)));
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
        Rock::new(vec![
            0b1111000,
        ]),
        Rock::new(vec![
            0b0100000,
            0b1110000,
            0b0100000,
        ]),
        Rock::new(vec![
            // "upside down" since we grow the chamber upwards
            0b1110000,
            0b0010000,
            0b0010000,
        ]),
        Rock::new(vec![
            0b1000000,
            0b1000000,
            0b1000000,
            0b1000000,
        ]),
        Rock::new(vec![
            0b1100000,
            0b1100000,
        ]),
    ]
}

fn is_at_rest(
    chamber: &Chamber,
    rock: &Rock,
    rock_x: usize,
    rock_y: usize,
) -> bool {
    if rock_y == 0 {
        return true;
    }
    rock.points.iter().enumerate().any(|(y, mask)| {
        let yp = y + rock_y;
        yp <= chamber.len() && yp > 0 && (chamber[yp - 1] & (mask >> rock_x) != 0)
    })
}

fn rock_collides(
    chamber: &Chamber,
    rock: &Rock,
    rock_x: usize,
    rock_y: usize,
) -> bool {
    rock.points.iter().enumerate().any(|(y, mask)| {
        let yp = y + rock_y;
        if yp >= chamber.len() {
            false
        } else {
            chamber[yp] & (mask >> rock_x) != 0
        }
    })
}

fn add_rock_to_chamber(
    chamber: &mut Chamber,
    rock: &Rock,
    rock_x: usize,
    rock_y: usize,
) {
    while rock_y + rock.points.len() > chamber.len() {
        chamber.push(0);
    }
    for (y, mask) in rock.points.iter().enumerate() {
        let yp = y + rock_y;
        chamber[yp] |= mask >> rock_x;
    }
}

// fn is_prunable(chamber: &Chamber, x: u8, y: usize) -> bool {
//     if y < CHAMBER_WIDTH as usize {
//         false
//     } else if x >= CHAMBER_WIDTH {
//         true
//     } else if chamber[y] & (0b1000000 >> x) == 0 {
//         false
//     } else {
//         is_prunable(chamber, x + 1, y) || 
//             is_prunable(chamber, x + 1, y + 1) || 
//             is_prunable(chamber, x + 1, y - 1)
//     }
// }
fn is_prunable(chamber: &Chamber, x: u8, y: usize) -> bool {
    if y < CHAMBER_WIDTH as usize {
        false
    } else if x >= CHAMBER_WIDTH {
        true
    } else if chamber[y] & (0b1000000 >> x) == 0 {
        false
    } else {
        is_prunable(chamber, x + 1, y) || 
            is_prunable(chamber, x + 1, y + 1) || 
            is_prunable(chamber, x + 1, y - 1)
    }
}

// Returns the number of rows pruned
fn prune_chamber(chamber: &mut Chamber) -> usize {
    if chamber.len() < CHAMBER_WIDTH as usize {
        return 0;
    }
    for y in (CHAMBER_WIDTH as usize..chamber.len() - CHAMBER_WIDTH as usize).rev() {
        if is_prunable(chamber, 0, y) {
            let prune_index = y - CHAMBER_WIDTH as usize;
            chamber.drain(..prune_index);
            return prune_index;
        }
    }
    0
}

fn clamp_x_movement(
    rock: &Rock,
    movement: Movement,
    rock_x: usize,
) -> usize {
    match movement {
        Movement::Left => if rock_x > 0 {
            rock_x - 1
        } else {
            0
        },
        Movement::Right => if rock_x + rock.width >= CHAMBER_WIDTH as usize {
            rock_x
        } else {
            rock_x + 1
        },
    }
}

fn solve(movements: &[Movement], rock_count: usize) -> usize {
    let mut move_index = 0;
    let rocks = create_rocks();
    // Lower indices are closer to the bottom
    let mut chamber = Vec::<u8>::new();
    let mut amount_pruned = 0;
    for i in 0..rock_count {
        let rock = &rocks[i % rocks.len()];
        let mut rock_x = 2;

        // Optimized first 3 moves, since we know it can't hit the floor or other rocks
        for _ in 0..3 {
            rock_x = clamp_x_movement(rock, movements[move_index], rock_x);
            move_index = (move_index + 1) % movements.len();
        }
        let mut rock_y = chamber.len();

        loop {
            let proposed_x = clamp_x_movement(rock, movements[move_index], rock_x);
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
    solve(movements, 10000000 * 10)
    //solve(movements, 1000000000000)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_at_rest() {
        let mut chamber = vec![];
        let rock_1 = Rock::new(vec![0b1111000]);
        let rock_2 = Rock::new(vec![
            0b0100000,
            0b1110000,
            0b01000000
        ]);
        assert!(is_at_rest(&chamber, &rock_1, 2, 0));
        assert!(!is_at_rest(&chamber, &rock_1, 2, 1));

        chamber.push(0b0011110);
        assert!(is_at_rest(&chamber, &rock_1, 2, 1));
        assert!(!is_at_rest(&chamber, &rock_1, 2, 2));

        assert!(is_at_rest(&chamber, &rock_2, 2, 1));
        assert!(!is_at_rest(&chamber, &rock_2, 2, 2));
        assert!(!is_at_rest(&chamber, &rock_2, 0, 1));
        assert!(is_at_rest(&chamber, &rock_2, 0, 0));
    }

    #[test]
    fn test_rock_collides() {
        let rock_1 = Rock::new(vec![0b1111000]);
        let rock_2 = Rock::new(vec![
            0b0100000,
            0b1110000,
            0b01000000
        ]);
        let mut chamber = vec![];
        chamber.push(0b0011110);

        assert!(rock_collides(&chamber, &rock_1, 2, 0));
        assert!(!rock_collides(&chamber, &rock_1, 2, 1));

        assert!(rock_collides(&chamber, &rock_2, 2, 0));
        assert!(!rock_collides(&chamber, &rock_2, 2, 1));

        assert!(!rock_collides(&chamber, &rock_2, 0, 0));
        assert!(!rock_collides(&chamber, &rock_2, 0, 1));
    }
}
