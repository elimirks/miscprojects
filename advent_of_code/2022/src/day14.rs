use crate::common::*;

const MID_X: usize = 500;

#[derive(Debug)]
struct Map {
    // Indicates if the given grid position is taken up
    grid: Vec<Vec<bool>>,
    dim: usize,
}

pub fn day14() -> AocResult<()> {
    println!("Part 1: {}", part1()?);
    println!("Part 2: {}", part2()?);
    Ok(())
}

fn part1() -> AocResult<usize> {
    let mut map = parse()?;
    let mut amount = 0;
    while let Some((rest_x, rest_y)) = drop_sand(&map) {
        map.grid[rest_y][rest_x] = true;
        amount += 1;
    }
    Ok(amount)
}

fn part2() -> AocResult<usize> {
    let mut map = parse()?;
    let lowest_y = map.grid.iter().enumerate()
        .filter(|(_, row)| row.iter().any(|b| *b))
        .map(|(y, _)| y)
        .max().unwrap();
    // Set the floor
    for x in 0..map.dim {
        map.grid[lowest_y + 2][x] = true;
    }
    let mut amount = 1; // Start at 1 for the initial sand point
    while let Some((rest_x, rest_y)) = drop_sand(&map) {
        map.grid[rest_y][rest_x] = true;
        amount += 1;
    }
    Ok(amount)
}

// Returns Some(point) if the sand will rest at that point
fn drop_sand(map: &Map) -> Option<(usize, usize)> {
    let mut sand_x = MID_X;
    let mut sand_y = 0;
    while in_bounds(map, sand_x, sand_y) {
        match check_below(map, sand_x, sand_y) {
            (true, true, true) => {
                return if sand_x == MID_X && sand_y == 0 {
                    // It's blocking the starting point! So return None
                    None
                } else {
                    Some((sand_x, sand_y))
                };
            },
            (false, true, _)   => {
                sand_y += 1;
                sand_x -= 1;
            },
            (_, true, false)   => {
                sand_y += 1;
                sand_x += 1;
            },
            _ => {
                sand_y += 1;
            },
        }
    }
    None
}

fn is_occupied(map: &Map, x: usize, y: usize) -> bool {
    y < map.dim && x < map.dim && map.grid[y][x]
}

fn check_below(map: &Map, x: usize, y: usize) -> (bool, bool, bool) {
    (is_occupied(map, x - 1, y + 1),
     is_occupied(map, x, y + 1),
     is_occupied(map, x + 1, y + 1))
}

fn in_bounds(map: &Map, x: usize, y: usize) -> bool {
    x < map.dim && y < map.dim
}

fn parse() -> AocResult<Map> {
    let content = std::fs::read_to_string("data/day14.txt")?;
    let rock_lines = content.lines().map(|line| {
        line.split(" -> ").map(|pair_str| {
            let pair_vec = pair_str.split(',')
                .map(|num| num.parse::<usize>().unwrap())
                .collect::<Vec<_>>();
            (pair_vec[0], pair_vec[1])
        }).collect::<Vec<_>>()
    }).collect::<Vec<_>>();
    // Should be big enough
    let dim = MID_X * 2;
    let mut grid = vec![vec![false; dim]; dim];
    for line in rock_lines {
        line.windows(2).for_each(|window| {
            let (x1, y1) = window[0];
            let (x2, y2) = window[1];
            let start_x  = x1.min(x2);
            let start_y  = y1.min(y2);
            let end_x    = x1.max(x2);
            let end_y    = y1.max(y2);
            for x in start_x..=end_x {
                grid[start_y][x] = true;
            }
            for y in start_y..=end_y {
                grid[y][start_x] = true;
            }
        });
    }
    Ok(Map {
        grid,
        dim,
    })
}
