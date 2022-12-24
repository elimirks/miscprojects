use std::collections::HashMap;

use num::integer::Roots;

use crate::common::*;

#[derive(Debug, PartialEq, Copy, Clone)]
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

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum Dir {
    R,
    L,
    U,
    D,
}

impl Dir {
    fn cw(&self) -> Dir {
        match self {
            Dir::R => Dir::D,
            Dir::L => Dir::U,
            Dir::U => Dir::R,
            Dir::D => Dir::L,
        }
    }

    fn ccw(&self) -> Dir {
        match self {
            Dir::R => Dir::U,
            Dir::L => Dir::D,
            Dir::U => Dir::L,
            Dir::D => Dir::R,
        }
    }
}

// Maps between (face ID, edge direction) pairs
type EdgeMapping = HashMap<(usize, Dir), (usize, Dir)>;

// (face index, x, y)
#[derive(Debug)]
struct Coord {
    face_index: usize,
    x: usize,
    y: usize,
}

struct Cube {
    // Stores faces<rows<tiles>>
    faces: Vec<Vec<Vec<Tile>>>,
    // Stores the positions of the faces, as if they were still flattened
    flat_face_positions: Vec<(usize, usize)>,
    // Width / height of each face
    dim: usize,
    // Maps between (face ID, edge direction) pairs
    edge_mapping: EdgeMapping,
}

impl Cube {
    // Assumes coord is in bounds
    fn is_wall(&self, coord: &Coord) -> bool {
        self.faces[coord.face_index][coord.y][coord.x] == Tile::Wall
    }
}

fn coord_in_direction(cube: &Cube, coord: Coord, from_dir: Dir) -> (Coord, Dir) {
    let should_wrap = match from_dir {
        Dir::R if coord.x == cube.dim - 1 => true,
        Dir::L if coord.x == 0            => true,
        Dir::U if coord.y == 0            => true,
        Dir::D if coord.y == cube.dim - 1 => true,
        _ => false,
    };
    if should_wrap {
        let from_face = coord.face_index;
        let (to_face, to_dir) = cube.edge_mapping.get(&(from_face, from_dir)).unwrap();

        let mut index = index_of_coord(coord.x, coord.y, from_dir);
        if should_reverse_face_coord(from_dir, *to_dir) {
            index = cube.dim - 1 - index;
        }
        let (x, y) = index_to_coord(index, cube.dim, *to_dir);
        (Coord {
            face_index: *to_face,
            x, y,
        }, to_dir.cw().cw())
    } else {
        let (x, y) = match from_dir {
            Dir::R => (coord.x + 1, coord.y),
            Dir::L => (coord.x - 1, coord.y),
            Dir::U => (coord.x, coord.y - 1),
            Dir::D => (coord.x, coord.y + 1),
        };
        (Coord {
            face_index: coord.face_index,
            x, y
        }, from_dir)
    }
}

// Returns the index along the given edge for the specified coord
// Assumes the coord is actually on that edge
fn index_of_coord(x: usize, y: usize, dir: Dir) -> usize {
    match dir {
        Dir::R | Dir::L => y,
        _               => x,
    }
}

fn index_to_coord(index: usize, dim: usize, dir: Dir) -> (usize, usize) {
    match dir {
        Dir::R => (dim - 1, index),
        Dir::L => (0, index),
        Dir::U => (index, 0),
        Dir::D => (index, dim - 1),
    }
}

fn should_reverse_face_coord(from: Dir, to: Dir) -> bool {
    match (from, to) {
        (from, to) if from == to => true,
        (Dir::U, Dir::R) => true,
        (Dir::L, Dir::D) => true,
        (Dir::R, Dir::U) => true,
        (Dir::D, Dir::L) => true,
        _ => false,
    }
}

#[derive(Debug)]
struct Player {
    x: usize,
    y: usize,
    // The direction the player is moving on the map face
    dir: Dir,
    // Only used for part 2
    face_index: usize,
}

impl Player {
    fn coord(&self) -> Coord {
        Coord { face_index: self.face_index, x: self.x, y: self.y }
    }

    fn facing_id(&self) -> usize {
        match self.dir {
            Dir::R => 0,
            Dir::D => 1,
            Dir::L => 2,
            Dir::U => 3,
        }
    }
}

pub fn day22() -> AocResult<()> {
    let (map, movements) = parse(&std::fs::read_to_string("data/day22.txt")?);
    println!("Part 1: {}", part1(&map, &movements));
    println!("Part 2: {}", part2(&map, &movements));
    Ok(())
}

fn part2(grid: &[Vec<Tile>], movements: &[Movement]) -> usize {
    let cube = grid_to_cube(grid);
    let mut player = Player {
        x: find_start_x(&cube.faces[0][0]),
        y: 0,
        face_index: 0,
        dir: Dir::R,
    };
    for movement in movements.iter() {
        match movement {
            Movement::Forward(amount) => {
                for _ in 0..*amount {
                    let (next_coord, next_dir) = coord_in_direction(&cube, player.coord(), player.dir);
                    if cube.is_wall(&next_coord) {
                        break;
                    }
                    player.x = next_coord.x;
                    player.y = next_coord.y;
                    player.face_index = next_coord.face_index;
                    player.dir = next_dir;
                }
            },
            Movement::Clockwise => player.dir = player.dir.cw(),
            Movement::Counterclockwise => player.dir = player.dir.ccw(),
        };
    }
    let (face_x, face_y) = cube.flat_face_positions[player.face_index];
    let x = face_x + player.x;
    let y = face_y + player.y;
    1000 * (y + 1) + 4 * (x + 1) + player.facing_id()
}

fn grid_to_cube(grid: &[Vec<Tile>]) -> Cube {
    let tile_count = grid.iter()
        .flatten()
        .filter(|tile| **tile != Tile::Void).count();
    let dim = (tile_count / 6).sqrt();

    // Normalized coordinates of each face
    let mut face_locs = vec![];
    let mut faces = vec![];
    let mut flat_face_positions = vec![];
    for y in (0..grid.len()).step_by(dim) {
        for x in (0..grid[y].len()).step_by(dim) {
            if grid[y][x] == Tile::Void {
                continue;
            }
            let mut face = vec![];
            for yi in 0..dim {
                let mut row = vec![];
                for xi in 0..dim {
                    row.push(grid[y + yi][x + xi]);
                }
                face.push(row);
            }
            faces.push(face);
            flat_face_positions.push((x, y));
            face_locs.push(((x / dim) as i8, (y / dim) as i8));
        }
    }
    Cube {
        dim,
        faces,
        flat_face_positions,
        edge_mapping: fold_faces(&face_locs),
    }
}

fn fold_faces(locs: &Vec<(i8, i8)>) -> EdgeMapping {
    let mut edge_mapping = HashMap::new();
    // Populate initial edge mapping
    for i in 0..locs.len() {
        let (x, y) = locs[i];
        if let Some(below_idx) = locs.iter().position(|point| *point == (x, y + 1)) {
            edge_mapping.insert((i, Dir::D), (below_idx, Dir::U));
            edge_mapping.insert((below_idx, Dir::U), (i, Dir::D));
        }
        if let Some(left_idx) = locs.iter().position(|point| *point == (x - 1, y)) {
            edge_mapping.insert((i, Dir::L), (left_idx, Dir::R));
            edge_mapping.insert((left_idx, Dir::R), (i, Dir::L));
        }
    }
    while try_joining_edges(&mut edge_mapping) {}
    edge_mapping
}

// Returns true if it found a new edge to join
fn try_joining_edges(mapping: &mut EdgeMapping) -> bool {
    for i in 0..6 {
        let mut dir = Dir::U;
        for _ in 0..4 {
            // If we know the mapping from one face to another in a given
            // direction, then any clockwise turns we make from there must
            // also be the clockwise edge of the first face.
            if let Some(conn) = mapping.get(&(i, dir)) {
                if let Some(conn_cw) = mapping.get(&(conn.0, conn.1.ccw())) {
                    let from = (i, dir.cw());
                    let to   = (conn_cw.0, conn_cw.1.ccw());
                    if !mapping.contains_key(&from) {
                        mapping.insert(from, to);
                        mapping.insert(to, from);
                        return true;
                    }
                }
            }
            dir = dir.cw();
        }
    }
    false
}

fn part1(map: &[Vec<Tile>], movements: &[Movement]) -> usize {
    let mut player = Player {
        x: find_start_x(&map[0]),
        y: 0,
        dir: Dir::R,
        // Not used for part 1, the value don't matter here
        face_index: 0,
    };
    for movement in movements.iter() {
        match movement {
            Movement::Forward(amount) => move_forward(map, &mut player, *amount),
            Movement::Clockwise => player.dir = player.dir.cw(),
            Movement::Counterclockwise => player.dir = player.dir.ccw(),
        }
    }
    1000 * (player.y + 1) + 4 * (player.x + 1) + player.facing_id()
}

fn move_forward(map: &[Vec<Tile>], player: &mut Player, amount: usize) {
    let (delta_x, delta_y) = match player.dir {
        Dir::R => (1, 0),
        Dir::L => (-1, 0),
        Dir::U => (0, -1),
        Dir::D => (0, 1),
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
        Dir::R => while is_in_bounds(map, x - 1, y) {
            x -= 1;
        },
        Dir::L => while is_in_bounds(map, x + 1, y) {
            x += 1;
        },
        Dir::U => while is_in_bounds(map, x, y + 1) {
            y += 1;
        },
        Dir::D => while is_in_bounds(map, x, y - 1) {
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
    (map, parse_movements(parts[1]))
}

fn parse_movements(content: &str) -> Vec<Movement> {
    let movement_chars = content.chars().collect::<Vec<_>>();
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
            c => panic!("Unexpected movement char: `{c}`")
        });
    }
    movements
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fold_faces() {
        let mapping = fold_faces(&vec![
            (2, 0),
            (0, 1),
            (1, 1),
            (2, 1),
            (2, 2),
            (3, 2),
        ]);
        // Check that all the disconnected edges are correct
        assert_eq!(Some(&(0, Dir::L)), mapping.get(&(2, Dir::U)));
        assert_eq!(Some(&(0, Dir::U)), mapping.get(&(1, Dir::U)));
        assert_eq!(Some(&(2, Dir::D)), mapping.get(&(4, Dir::L)));
        assert_eq!(Some(&(1, Dir::D)), mapping.get(&(4, Dir::D)));
        assert_eq!(Some(&(1, Dir::L)), mapping.get(&(5, Dir::D)));
        assert_eq!(Some(&(0, Dir::R)), mapping.get(&(5, Dir::R)));
        assert_eq!(Some(&(3, Dir::R)), mapping.get(&(5, Dir::U)));
        // Sanity check
        assert_eq!(24, mapping.len());
    }
}
