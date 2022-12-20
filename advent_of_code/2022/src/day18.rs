use std::collections::HashSet;

use crate::common::*;
use sscanf::sscanf;

// This solution is really gnarly.... 
// But I can't think of a better way to write something performant but dense

pub fn day18() -> AocResult<()> {
    let points = std::fs::read_to_string("data/day18.txt")?
        .lines()
        .map(|line| {
            sscanf!(line, "{usize},{usize},{usize}").unwrap()
        })
        .collect::<Vec<_>>();
    println!("Part 1: {}", part1(&points));
    println!("Part 2: {}", part2(&points));
    Ok(())
}

fn part1(unshifted_points: &[(usize, usize, usize)]) -> usize {
    let points = unshifted_points.iter().map(|&(x, y, z)| {
        // So that we don't need to bounds-check
        (x + 1, y + 1, z + 1)
    }).collect::<Vec<_>>();
    let max_x = points.iter().map(|it| it.0).max().unwrap();
    let max_y = points.iter().map(|it| it.1).max().unwrap();
    let max_z = points.iter().map(|it| it.2).max().unwrap();
    let mut grid = vec![vec![vec![false; max_z + 2]; max_y + 2]; max_x + 2];
    // Populate the grid
    for point in points.iter() {
        grid[point.0][point.1][point.2] = true;
    }
    let mut untouched_sides = 0;
    for x in 1..=max_x {
        for y in 1..=max_y {
            for z in 1..=max_z {
                if !grid[x][y][z] {
                    continue;
                }
                if !grid[x - 1][y][z] {
                    untouched_sides += 1;
                }
                if !grid[x + 1][y][z] {
                    untouched_sides += 1;
                }
                if !grid[x][y - 1][z] {
                    untouched_sides += 1;
                }
                if !grid[x][y + 1][z] {
                    untouched_sides += 1;
                }
                if !grid[x][y][z - 1] {
                    untouched_sides += 1;
                }
                if !grid[x][y][z + 1] {
                    untouched_sides += 1;
                }
            }
        }
    }
    untouched_sides
}

fn part2(unshifted_points: &[(usize, usize, usize)]) -> usize {
    let points = unshifted_points.iter().map(|&(x, y, z)| {
        // So that we don't need to bounds-check
        (x + 1, y + 1, z + 1)
    }).collect::<Vec<_>>();
    let max_x = points.iter().map(|it| it.0).max().unwrap();
    let max_y = points.iter().map(|it| it.1).max().unwrap();
    let max_z = points.iter().map(|it| it.2).max().unwrap();
    let mut grid = vec![vec![vec![false; max_z + 2]; max_y + 2]; max_x + 2];
    // Populate the grid
    for point in points.iter() {
        grid[point.0][point.1][point.2] = true;
    }
    // Perform DFS to find the entire surface area
    let mut to_visit = vec![(0, 0, 0)];
    let mut visited = HashSet::new();

    let mut external_sides = 0;
    while let Some((x, y, z)) = to_visit.pop() {
        if visited.contains(&(x, y, z)) {
            continue;
        }
        // Don't iterate through walls
        if grid[x][y][z] {
            continue;
        }
        // Side check
        if x > 0 && grid[x - 1][y][z] {
            external_sides += 1;
        }
        if x < max_x + 1 && grid[x + 1][y][z] {
            external_sides += 1;
        }
        if y > 0 && grid[x][y - 1][z] {
            external_sides += 1;
        }
        if y < max_y + 1 && grid[x][y + 1][z] {
            external_sides += 1;
        }
        if z > 0 && grid[x][y][z - 1] {
            external_sides += 1;
        }
        if z < max_z + 1 && grid[x][y][z + 1] {
            external_sides += 1;
        }
        // Continue DFS
        if x > 0 {
            to_visit.push((x - 1, y, z));
        }
        if x < max_x + 1 {
            to_visit.push((x + 1, y, z));
        }
        if y > 0 {
            to_visit.push((x, y - 1, z));
        }
        if y < max_y + 1 {
            to_visit.push((x, y + 1, z));
        }
        if z > 0 {
            to_visit.push((x, y, z - 1));
        }
        if z < max_z + 1 {
            to_visit.push((x, y, z + 1));
        }
        visited.insert((x, y, z));
    }
    external_sides
}
