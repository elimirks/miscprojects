use crate::common::*;

#[derive(Debug, PartialEq)]
enum Tile {
    Wall,
    Path,
    Void,
}

#[derive(Debug)] enum Movement {
    Forward(usize),
    Clockwise,
    Counterclockwise,
}

#[derive(Debug, Copy, Clone)]
enum Direction {
    Right,
    Left,
    Up,
    Down,
}

impl Direction {
    fn cw(&self) -> Direction {
        match self {
            Direction::Right => Direction::Down,
            Direction::Left => Direction::Up,
            Direction::Up => Direction::Right,
            Direction::Down => Direction::Left,
        }
    }

    fn ccw(&self) -> Direction {
        match self {
            Direction::Right => Direction::Up,
            Direction::Left => Direction::Down,
            Direction::Up => Direction::Left,
            Direction::Down => Direction::Right,
        }
    }
}

#[derive(Debug)]
struct Player {
    x: usize,
    y: usize,
    dir: Direction,
}

pub fn day22() -> AocResult<()> {
    let (map, movements) = parse(&std::fs::read_to_string("data/day22.txt")?);
    println!("Part 1: {}", part1(&map, &movements));
    Ok(())
}

fn part1(map: &[Vec<Tile>], movements: &[Movement]) -> usize {
    let mut player = Player {
        x: find_start_x(&map[0]),
        y: 0,
        dir: Direction::Right,
    };
    for movement in movements.iter() {
        match movement {
            Movement::Forward(amount) => move_forward(map, &mut player, *amount),
            Movement::Clockwise => player.dir = player.dir.cw(),
            Movement::Counterclockwise => player.dir = player.dir.ccw(),
        }
    }

    let facing_id = match player.dir {
        Direction::Right => 0,
        Direction::Down  => 1,
        Direction::Left  => 2,
        Direction::Up    => 3,
    };
    1000 * (player.y + 1) + 4 * (player.x + 1) + facing_id
}

fn move_forward(map: &[Vec<Tile>], player: &mut Player, amount: usize) {
    let (delta_x, delta_y) = match player.dir {
        Direction::Right => (1, 0),
        Direction::Left => (-1, 0),
        Direction::Up => (0, -1),
        Direction::Down => (0, 1),
    };
    for _ in 0..amount {
        let x = player.x as i64 + delta_x;
        let y = player.y as i64 + delta_y;

        if is_in_bounds(map, x, y) {
            if map[y as usize][x as usize] == Tile::Wall {
                break;
            }
            player.x = x as usize;
            player.y = y as usize;
        } else {
            wrap_forward(map, player);
        }
    }
}

// Attempts to cycle around a portion of the map
// Used for if the player hits an edge
fn wrap_forward(map: &[Vec<Tile>], player: &mut Player) {
    let mut x = player.x as i64;
    let mut y = player.y as i64;
    match player.dir {
        Direction::Right => while is_in_bounds(map, x - 1, y) {
            x -= 1;
        },
        Direction::Left => while is_in_bounds(map, x + 1, y) {
            x += 1;
        },
        Direction::Up => while is_in_bounds(map, x, y + 1) {
            y += 1;
        },
        Direction::Down => while is_in_bounds(map, x, y - 1) {
            y -= 1;
        },
    }
    if map[y as usize][x as usize] == Tile::Path {
        player.x = x as usize;
        player.y = y as usize;
    }
}

fn is_in_bounds(map: &[Vec<Tile>], x: i64, y: i64) -> bool {
    if x < 0 || y < 0 || x >= map[0].len() as i64 || y >= map.len() as i64 {
        false
    } else {
        map[y as usize][x as usize] != Tile::Void
    }
}

fn find_start_x(row: &[Tile]) -> usize {
    row.iter().position(|tile| *tile == Tile::Path).expect("No starting tile!")
}

fn parse(content: &str) -> (Vec<Vec<Tile>>, Vec<Movement>) {
    let parts = content.split("\n\n").collect::<Vec<_>>();
    let mut map = parts[0].lines().map(|line| {
        line.chars().map(|c| match c {
            ' ' => Tile::Void,
            '.' => Tile::Path,
            '#' => Tile::Wall,
            c   => panic!("Unexpected map char: `{c}`")
        }).collect::<Vec<_>>()
    }).collect::<Vec<_>>();

    let max_row_len = map.iter().map(|row| row.len()).max().unwrap();
    for row in map.iter_mut() {
        while row.len() < max_row_len {
            row.push(Tile::Void);
        }
    }
    let movement_chars = parts[1].chars().collect::<Vec<_>>();
    let mut movements = vec![];
    let mut i = 0;
    while i < movement_chars.len() {
        movements.push(match movement_chars[i] {
            'R' => {
                i += 1;
                Movement::Clockwise
            },
            'L' => {
                i += 1;
                Movement::Counterclockwise
            },
            c if c.is_ascii_digit() => {
                let mut num = 0;
                while i < movement_chars.len() && movement_chars[i].is_ascii_digit() {
                    num *= 10;
                    num += movement_chars[i].to_digit(10).unwrap();
                    i += 1;
                }
                Movement::Forward(num as usize)
            },
            '\n' => {
                break;
            }
            c    => panic!("Unexpected movement char: `{c}`")
        });
    }
    (map, movements)
}
