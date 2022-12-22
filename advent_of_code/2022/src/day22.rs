use crate::common::*;

enum Tile {
    Wall,
    Path,
    Void,
}

enum Movement {
    Forward(usize),
    Clockwise,
    Counterclockwise,
}

enum Direction {
    Right,
    Left,
    Up,
    Down,
}

struct Player {
    x: usize,
    y: usize,
    direction: Direction,
}

pub fn day22() -> AocResult<()> {
    let (map, movements) = parse(&std::fs::read_to_string("data/day22.txt")?);
    println!("Part 1: {}", part1(&map, &movements));
    Ok(())
}

fn part1(map: &Vec<Vec<Tile>>, movements: &[Movement]) -> usize {
    unimplemented!()
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
            'R'  => Movement::Clockwise,
            'L'  => Movement::Counterclockwise,
            c if c.is_ascii_digit() => {
                let mut num = 0;
                while i < movement_chars.len() && movement_chars[i].is_ascii_digit() {
                    num *= 10;
                    num += movement_chars[i].to_digit(10).unwrap();
                    i += 1;
                }
                Movement::Forward(num as usize)
            },
            '\n' => break,
            c    => panic!("Unexpected movement char: `{c}`")
        });
    }
    (map, movements)
}
