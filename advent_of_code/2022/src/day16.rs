use std::collections::{HashMap, HashSet, VecDeque};

use crate::common::*;
use bit_set::BitSet;
use itertools::Itertools;
use sscanf::sscanf;

#[derive(Clone)]
struct Room {
    flow_rate: i64,
    // Maps to the index of the room
    tunnels: Vec<usize>,
}

pub fn day16() -> AocResult<()> {
    let rooms = parse()?;
    let distances = create_distance_matrix(&rooms);
    let valve_rooms = rooms.iter().enumerate().filter_map(|(index, room)| {
        if room.flow_rate == 0 {
            None
        } else {
            Some(index)
        }
    }).collect::<Vec<usize>>();
    let flow_rates = rooms.iter()
        .map(|room| room.flow_rate)
        .collect::<Vec<_>>();

    println!("Part 1: {}", time_closure(|| part1(&distances, &valve_rooms, &flow_rates)));
    println!("Part 2: {}", time_closure(|| part2(&distances, &valve_rooms, &flow_rates)));
    Ok(())
}

fn part1(
    distances: &[Vec<i64>],
    valve_rooms: &[usize],
    flow_rates: &[i64],
) -> i64 {
    let unvisited = valve_rooms.iter().cloned().collect::<BitSet<_>>();
    max_release(0, 30, unvisited, distances, flow_rates)
}

fn part2(
    distances: &[Vec<i64>],
    valve_rooms: &[usize],
    flow_rates: &[i64],
) -> i64 {
    let time_limit = 26;
    (1..(valve_rooms.len() / 2)).flat_map(|k| {
        valve_rooms.iter().combinations(k).map(|my_valves| {
            let my_unvisited = my_valves.iter()
                .map(|&&index| index)
                .collect::<BitSet<_>>();
            let elephant_unvisited = valve_rooms.iter()
                .filter(|&&room_idx| !my_unvisited.contains(room_idx))
                .cloned()
                .collect::<BitSet<_>>();

            let my_release = 
                max_release(0, time_limit, my_unvisited, distances, flow_rates);
            let elephant_release =
                max_release(0, time_limit, elephant_unvisited, distances, flow_rates);
            my_release + elephant_release
        })
    }).max().expect("No rooms given")
}

fn max_release(
    index: usize,
    t_remaining: i64,
    unvisited: BitSet,
    distances: &[Vec<i64>],
    flow_rates: &[i64],
) -> i64 {
    if unvisited.is_empty() || t_remaining <= 0 {
        return 0;
    }
    unvisited.iter().map(|next| {
        let t_travel = distances[index][next];
        if t_remaining > t_travel {
            let mut next_unvisited = unvisited.clone();
            next_unvisited.remove(next);
            flow_rates[next] * (t_remaining - t_travel - 1) + max_release(
                next,
                t_remaining - t_travel - 1,
                next_unvisited,
                distances,
                flow_rates,
            )
        } else {
            0
        }
    }).max().unwrap()
}

fn create_distance_matrix(rooms: &Vec<Room>) -> Vec<Vec<i64>> {
    let dim = rooms.len();
    let mut distances = vec![vec![i64::MAX; dim]; dim];
    for i in 0..dim {
        populate_row_distances(rooms, &mut distances, i);
    }
    distances
}

// Populates distances for a row. Guarantees to have the shortest distances
fn populate_row_distances(
    rooms: &[Room],
    distances: &mut [Vec<i64>],
    index: usize,
) {
    let mut to_visit = VecDeque::<(usize, i64)>::new();
    let mut visited = HashSet::<usize>::new();
    to_visit.push_back((index, 0));

    while let Some((next_index, next_dist)) = to_visit.pop_front() {
        if visited.contains(&next_index) {
            continue;
        }
        for &tunnel in rooms[next_index].tunnels.iter() {
            to_visit.push_back((tunnel, next_dist + 1));
        }
        distances[index][next_index] = next_dist;
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
