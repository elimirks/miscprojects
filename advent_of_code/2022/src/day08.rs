use crate::common::*;

pub fn day08() -> AocResult<()> {
    println!("Part 1: {}", part1()?);
    println!("Part 1: {}", part1_golf()?);
    println!("Part 2: {}", part2()?);
    Ok(())
}

fn part1() -> AocResult<usize> {
    let height_grid = parse()?;
    let height = height_grid.len();
    let width = height_grid[0].len();
    let mut visible_grid = vec![vec![false; width]; height];

    // Visible from the left
    for y in 0..height {
        let mut current_height = -1;
        for x in 0..width {
            let tree_height = height_grid[y][x];
            if tree_height > current_height {
                current_height = tree_height;
                visible_grid[y][x] = true;
            }
        }
    }
    // Visible from the right
    for y in 0..height {
        let mut current_height = -1;
        for x in (0..width).rev() {
            let tree_height = height_grid[y][x];
            if tree_height > current_height {
                current_height = tree_height;
                visible_grid[y][x] = true;
            }
        }
    }
    // Visible from the top
    for x in 0..width {
        let mut current_height = -1;
        for y in 0..height {
            let tree_height = height_grid[y][x];
            if tree_height > current_height {
                current_height = tree_height;
                visible_grid[y][x] = true;
            }
        }
    }
    // Visible from the bottom
    for x in 0..width {
        let mut current_height = -1;
        for y in (0..height).rev() {
            let tree_height = height_grid[y][x];
            if tree_height > current_height {
                current_height = tree_height;
                visible_grid[y][x] = true;
            }
        }
    }
    Ok(visible_grid.iter().map(|row| row.iter().filter(|x| **x).count()).sum())
}

fn populate_visibility(
    height_grid: &Vec<Vec<i8>>,
    visible_grid: &mut Vec<Vec<bool>>,
    swap_dims: bool,
    invert_dim2: bool,
) {
    let dim = visible_grid.len();
    for dim1 in 0..dim {
        let mut current_height = -1;
        for dim2 in 0..dim {
            let (x, y) = match (swap_dims, invert_dim2) {
                (true, true) => (dim - 1 - dim2, dim1),
                (true, false) => (dim2, dim1),
                (false, true) => (dim1, dim - 1 - dim2),
                (false, false) => (dim1, dim2),
            };
            let tree_height = height_grid[y][x];
            if tree_height > current_height {
                current_height = tree_height;
                visible_grid[y][x] = true;
            }
        }
    }
}

fn part1_golf() -> AocResult<usize> {
    let height_grid = parse()?;
    let dim = height_grid.len();
    assert!(height_grid[0].len() == dim);
    let mut visible_grid = vec![vec![false; dim]; dim];
    populate_visibility(&height_grid, &mut visible_grid, false, false);
    populate_visibility(&height_grid, &mut visible_grid, false, true);
    populate_visibility(&height_grid, &mut visible_grid, true, false);
    populate_visibility(&height_grid, &mut visible_grid, true, true);
    Ok(visible_grid.iter().map(|row| row.iter().filter(|x| **x).count()).sum())
}

fn part2() -> AocResult<usize> {
    let grid = parse()?;
    let height = grid.len();
    let width = grid[0].len();
    let mut max = 0;
    for y in 0..height {
        for x in 0..width {
            max = std::cmp::max(max, scenic_score(x, y, &grid));
        }
    }
    Ok(max)
}

fn scenic_score(x: usize, y: usize, grid: &Vec<Vec<i8>>) -> usize {
    let height = grid.len();
    let width = grid[0].len();
    let tree_height = grid[y][x];
    let mut right_score = 0;
    let mut left_score = 0;
    let mut up_score = 0;
    let mut down_score = 0;

    // Look right
    for x in (x+1)..width {
        right_score += 1;
        if grid[y][x] >= tree_height {
            break;
        }
    }
    // Look left
    for x in (0..x).rev() {
        left_score += 1;
        if grid[y][x] >= tree_height {
            break;
        }
    }
    // Look down
    for y in (y+1)..height {
        down_score += 1;
        if grid[y][x] >= tree_height {
            break;
        }
    }
    // Look up
    for y in (0..y).rev() {
        up_score += 1;
        if grid[y][x] >= tree_height {
            break;
        }
    }
    right_score * left_score * up_score * down_score
}

fn parse() -> AocResult<Vec<Vec<i8>>> {
    let raw = std::fs::read_to_string("data/day08.txt")?;
    Ok(raw.lines().map(|line| {
        line.chars().map(|c| c.to_digit(10).unwrap() as i8).collect::<Vec<_>>()
    }).collect())
}
