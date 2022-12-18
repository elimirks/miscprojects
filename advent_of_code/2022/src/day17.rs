use crate::common::*;

#[derive(Debug)]
enum Movement {
    Left,
    Right,
}

struct Rock {
    width: usize,
    height: usize,
    points: Vec<(usize, usize)>,
}

pub fn day17() -> AocResult<()> {
    let data = std::fs::read_to_string("data/day17.txt")?.chars()
        .filter_map(|c| match c {
            '>' => Some(Movement::Right),
            '<' => Some(Movement::Left),
            _   => None
        })
        .collect::<Vec<_>>();
    println!("Part 1: {}", part1(&data));
    Ok(())
}

fn create_rocks() -> Vec<Rock> {
    vec![
        Rock {
            width: 4,
            height: 1,
            points: vec![(0, 0), (1, 0), (2, 0), (3, 0)]
        },
        Rock {
            width: 3,
            height: 3,
            points: vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
        },
        Rock {
            width: 3,
            height: 3,
            // "upside down" since we grow the chamber upwards
            points: vec![(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
        },
        Rock {
            width: 1,
            height: 4,
            points: vec![(0, 1), (0, 2), (0, 3), (0, 4)]
        },
        Rock {
            width: 2,
            height: 2,
            points: vec![(0, 0), (1, 0), (0, 1), (1, 1)]
        },
    ]
}

fn is_at_rest(
    chamber: &Vec<Vec<bool>>,
    rock: &Rock,
    rock_x: usize,
    rock_y: usize,
) -> bool {
    rock.points.iter().map(|&(x, y)| {
        (x + rock_x, y + rock_y)
    }).all(|(x, y)| {
        if y == 0 {
            true
        } else if y > chamber.len() {
            false
        } else {
            chamber[y - 1][x]
        }
    })
}

fn print_chamber(chamber: &Vec<Vec<bool>>) {
    println!("-------");
    chamber.iter().rev().for_each(|row| {
        println!("{}", row.iter().map(|&value| match value {
            true  => '#',
            false => '.',
        }).collect::<String>());
    });
    println!("-------");
}

fn part1(movements: &[Movement]) -> i64 {
    let chamber_width = 7;
    let mut move_index = 0;
    let rocks = create_rocks();
    let _rock_count = 2022;
    // Lower indices are closer to the bottom
    let mut chamber = Vec::<Vec<bool>>::new();
    for i in 0..2 {
        let rock = &rocks[i % rocks.len()];
        let mut rock_x = 2;
        let mut rock_y = chamber.len() + rock.height + 2;

        loop {
            println!("{rock_x},{rock_y}");
            println!("{:?}", movements[move_index]);
            match movements[move_index] {
                Movement::Left => {
                    if rock_x > 0 {
                        rock_x -= 1;
                    }
                },
                Movement::Right => {
                    if rock_x + rock.width < chamber_width {
                        rock_x += 1;
                    }
                },
            }
            move_index = (move_index + 1) % movements.len();
            if rock_y == 0 || is_at_rest(&chamber, rock, rock_x, rock_y) {
                break;
            } else {
                rock_y -= 1;
            }
        }
        println!("{rock_x},{rock_y}");

        for &(x, y) in rock.points.iter() {
            let xp = x + rock_x;
            let yp = y + rock_y;
            while yp >= chamber.len() {
                chamber.push(vec![false; chamber_width]);
            }
            chamber[yp][xp] = true;
        }

        print_chamber(&chamber);
    }
    0
}
