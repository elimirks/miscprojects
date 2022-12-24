use std::collections::HashSet;
use crate::common::*;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Dir {
    U,
    D,
    L,
    R,
}

#[derive(Clone)]
struct Blizzard {
    dir: Dir,
    x: usize,
    y: usize,
}

pub fn day24() -> AocResult<()> {
    let (grid, blizzards) = parse(&std::fs::read_to_string("data/day24.txt")?);
    println!("Part 1: {}", part1(&grid, &blizzards));
    println!("Part 2: {}", part2(&grid, &blizzards));
    Ok(())
}

fn part1(grid: &Vec<Vec<bool>>, blizzards: &[Blizzard]) -> usize {
    let time_grid = create_time_grid(grid, blizzards);
    let start = (1, 0);
    let end   = (grid[0].len() - 2, grid.len() - 1);
    time_to_reach(&time_grid, start, end, 0) - 1
}

fn part2(grid: &Vec<Vec<bool>>, blizzards: &[Blizzard]) -> usize {
    let time_grid = create_time_grid(grid, blizzards);
    let start = (1, 0);
    let end   = (grid[0].len() - 2, grid.len() - 1);

    let t1 = time_to_reach(&time_grid, start, end, 0);
    let t2 = time_to_reach(&time_grid, end, start, t1);
    let t3 = time_to_reach(&time_grid, start, end, t2);
    t3 - 1
}

fn time_to_reach(
    time_grid: &Vec<Vec<Vec<bool>>>,
    start: (usize, usize),
    end: (usize, usize),
    mut t: usize,
) -> usize {
    let mut coords = HashSet::new();
    coords.insert(start);
    while !coords.contains(&end) {
        let next_grid = &time_grid[t % time_grid.len()];
        coords = next_possible_coords(next_grid, coords);
        t += 1;
    }
    t
}

fn next_possible_coords(
    next_grid: &Vec<Vec<bool>>,
    previous_coords: HashSet<(usize, usize)>
) -> HashSet<(usize, usize)> {
    let mut next = HashSet::new();
    for &(x, y) in previous_coords.iter() {
        // Down
        if y < next_grid.len() - 1 && !next_grid[y + 1][x] {
            next.insert((x, y + 1));
        }
        // Up
        if y > 0 && !next_grid[y - 1][x] {
            next.insert((x, y - 1));
        }
        // Left
        if !next_grid[y][x - 1] {
            next.insert((x - 1, y));
        }
        // Right
        if !next_grid[y][x + 1] {
            next.insert((x + 1, y));
        }
        // Always allow waiting if possible
        if !next_grid[y][x] {
            next.insert((x, y));
        }
    }
    next
}

// Returns a list of grids. Each entry represents a slice in time
fn create_time_grid(
    grid: &Vec<Vec<bool>>,
    initial_blizzards: &[Blizzard],
) -> Vec<Vec<Vec<bool>>> {
    let mut blizzards = initial_blizzards.to_owned();
    let mut time_grid = vec![];
    let height = grid.len() - 2;
    let width = grid[0].len() - 2;
    // Iterate up to the area, which is the maximum period of all blizzards
    let area = height * width;

    for _ in 0..area {
        let mut g = grid.clone();
        for bliz in blizzards.iter() {
            g[bliz.y][bliz.x] = true;
        }
        time_grid.push(g.clone());
        tick_blizzards(&mut blizzards, width, height);
    }
    time_grid
}

fn tick_blizzards(
    blizzards: &mut [Blizzard],
    width: usize,
    height: usize,
) {
    for bliz in blizzards.iter_mut() {
        match bliz.dir {
            Dir::U => bliz.y -= 1,
            Dir::D => bliz.y += 1,
            Dir::L => bliz.x -= 1,
            Dir::R => bliz.x += 1,
        };
        if bliz.x == 0 {
            bliz.x = width;
        } else if bliz.y == 0 {
            bliz.y = height;
        } else if bliz.x == width + 1 {
            bliz.x = 1;
        } else if bliz.y == height + 1 {
            bliz.y = 1;
        }
    }
}

fn parse(s: &str) -> (Vec<Vec<bool>>, Vec<Blizzard>) {
    let grid = s.lines().map(|line| {
        line.chars().map(|c| c == '#').collect::<Vec<_>>()
    }).collect::<Vec<_>>();
    let blizzards = s.lines().enumerate().flat_map(|(y, line)| {
        line.chars().enumerate().filter_map(move |(x, c)| {
            let dir = match c {
                '>' => Some(Dir::R),
                '<' => Some(Dir::L),
                '^' => Some(Dir::U),
                'v' => Some(Dir::D),
                _   => None,
            };
            dir.map(|dir| Blizzard {
                dir,
                x,
                y,
            })
        })
    }).collect::<Vec<_>>();
    (grid, blizzards)
}
