use itertools::Itertools;

use crate::common::*;

struct Map {
    altitudes: Vec<Vec<u8>>,
    distances: Vec<Vec<usize>>,
    height: usize,
    width: usize,
    start: (usize, usize),
    end: (usize, usize),
}

pub fn day12() -> AocResult<()> {
    println!("Part 1: {}", part1()?);
    println!("Part 2: {}", part2()?);
    Ok(())
}

fn part1() -> AocResult<usize> {
    let mut map = parse()?;
    let (x, y) = map.start;
    map.distances[y][x] = 0;
    populate_dp(&mut map, x, y);
    Ok(map.distances[map.end.1][map.end.0])
}

fn part2() -> AocResult<usize> {
    let mut map = parse()?;
    (0..map.height).cartesian_product(0..map.width).for_each(|(y, x)| {
        if map.altitudes[y][x] == 0 {
            map.distances[y][x] = 0;
            populate_dp(&mut map, x, y);
        }
    });
    Ok(map.distances[map.end.1][map.end.0])
}

fn populate_dp(map: &mut Map, x: usize, y: usize) {
    let dist = map.distances[y][x];
    let alt  = map.altitudes[y][x];
    if x < map.width - 1 {
        populate_dp_goto_next(map, x + 1, y, dist, alt);
    }
    if x > 0 {
        populate_dp_goto_next(map, x - 1, y, dist, alt);
    }
    if y < map.height - 1 {
        populate_dp_goto_next(map, x, y + 1, dist, alt);
    }
    if y > 0 {
        populate_dp_goto_next(map, x, y - 1, dist, alt);
    }
}

fn populate_dp_goto_next(map: &mut Map, x: usize, y: usize, dist: usize, alt: u8) {
    if map.altitudes[y][x] > alt + 1 {
        return;
    }
    let prev_dist = map.distances[y][x];
    let new_dist = prev_dist.min(dist + 1);
    if prev_dist != new_dist {
        map.distances[y][x] = new_dist;
        populate_dp(map, x, y);
    }
}

fn parse() -> AocResult<Map> {
    let input = std::fs::read_to_string("data/day12.txt")?;
    let mut start = (0, 0);
    let mut end   = (0, 0);
    let altitudes = input.lines().enumerate().map(|(y, line)| {
        line.chars().enumerate().map(|(x, c)| {
            match c {
                'E' => {
                    end = (x, y);
                    b'z' - b'a'
                },
                'S' => {
                    start = (x, y);
                    0
                },
                c => (c as u8) - b'a',
            }
        }).collect::<Vec<_>>()
    }).collect::<Vec<Vec<u8>>>();
    let height = altitudes.len();
    let width = altitudes[0].len();
    Ok(Map {
        height,
        width,
        altitudes,
        distances: vec![vec![usize::MAX; width]; height],
        start,
        end,
    })
}
