use std::{collections::{HashMap, HashSet, VecDeque}, rc::Rc};

use crate::common::*;
use sscanf::sscanf;

struct Node {
    index: usize,
    next: Option<Rc<Node>>,
}

impl Node {
    fn cons(index: usize, next: &Option<Rc<Node>>) -> Option<Rc<Node>> {
        Some(Rc::new(Node {
            index,
            next: next.clone(),
        }))
    }
}

#[derive(Clone)]
struct Room {
    flow_rate: i64,
    // Maps to the index of the room
    tunnels: Vec<usize>,
}

pub fn day16() -> AocResult<()> {
    let rooms = parse()?;
    println!("Part 1: {}", part1(&rooms));
    Ok(())
}

fn part1(rooms: &Vec<Room>) -> i64 {
    let matrix = create_distance_matrix(rooms);
    for row in matrix.iter() {
        for &dist in row.iter() {
            if dist == i64::MAX {
                print!(" max");
            } else {
                print!("{dist: >4}");
            }
        }
        println!();
    }
    unimplemented!()
}

// TODO: Compute a distance matrix. How many steps to arrive at each node, from each node?
// Then, iterate over the valves that have non-zero flow rate, and BAM.
// /Maybe/ this will be fast enough
fn create_distance_matrix(rooms: &Vec<Room>) -> Vec<Vec<i64>> {
    let dim = rooms.len();
    let mut distances = vec![vec![i64::MAX; dim]; dim];
    for i in 0..dim {
        distances[i][i] = 0;
    }
    // Populate initial distances
    for i in 0..dim {
        for &tunnel in rooms[i].tunnels.iter() {
            distances[i][tunnel] = 1;
        }
    }
    populate_row_distances(rooms, &mut distances, 0);
    distances
}

// Populates distances for a row. Guarantees to have the shortest distances
fn populate_row_distances(rooms: &Vec<Room>, distances: &mut Vec<Vec<i64>>, index: usize) {
    let mut to_visit = VecDeque::<(usize, usize)>::new();
    let mut visited = HashSet::<usize>::new();
    to_visit.push_back((index, 0));

    while let Some((next_index, next_dist)) = to_visit.pop_front() {
        if visited.contains(&next_index) {
            break;
        }
        // TODO: Breadth first traversal
        visited.insert(next_index);
    }
}

fn parse() -> AocResult<Vec<Room>> {
    let content = std::fs::read_to_string("data/day16.txt")?;
    let mut lines = content.lines().map(|line| {
        let (name, flow_rate, to_valves_str) =
            if let Ok(res) = sscanf!(line, "Valve {String} has flow rate={i64}; tunnels lead to valves {String}") {
                res
            } else {
                sscanf!(line, "Valve {String} has flow rate={i64}; tunnel leads to valve {String}").unwrap()
            };
        let tunnels = to_valves_str
            .split(", ")
            .map(|s| s.to_owned())
            .collect::<Vec<String>>();
        (name, flow_rate, tunnels)
    }).collect::<Vec<_>>();
    // So that AA will be the first in the list
    lines.sort_by_key(|line| line.0.clone());

    // Map of room name -> room index
    let mut index_map = HashMap::new();
    for (index, line) in lines.iter().enumerate() {
        index_map.insert(line.0.clone(), index);
    }
    Ok(lines.iter().map(|line| {
        Room {
            flow_rate: line.1,
            tunnels: line.2.iter().map(|name| {
                *index_map.get(name).unwrap()
            }).collect::<Vec<_>>(),
        }
    }).collect::<Vec<_>>())
}
