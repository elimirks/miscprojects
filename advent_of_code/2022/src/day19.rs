use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::common::*;
use sscanf::sscanf;
use rayon::prelude::*;

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
enum Mineral {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
struct MineralMap {
    ore: i32,
    clay: i32,
    obsidian: i32,
    geode: i32,
}

impl MineralMap {
    fn get(&self, mineral: Mineral) -> i32 {
        match mineral {
            Mineral::Ore => self.ore,
            Mineral::Clay => self.clay,
            Mineral::Obsidian => self.obsidian,
            Mineral::Geode => self.geode,
        }
    }

    fn add(&mut self, mineral: Mineral, to_add: i32) {
        match mineral {
            Mineral::Ore => self.ore += to_add,
            Mineral::Clay => self.clay += to_add,
            Mineral::Obsidian => self.obsidian += to_add,
            Mineral::Geode => self.geode += to_add,
        }
    }
}

struct Blueprint {
    id: i32,
    // robot type -> cost types
    robot_costs: HashMap<Mineral, Vec<(Mineral, i32)>>,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
struct Resources {
    minerals: MineralMap,
    // Robot counts
    robots: MineralMap,
}

pub fn day19() -> AocResult<()> {
    let blueprints = std::fs::read_to_string("data/day19.txt")?
        .lines()
        .map(|line| {
            let (
                id,
                ore_robot_cost,
                clay_robot_cost,
                obsidian_robot_cost_ore,
                obsidian_robot_cost_clay,
                geode_robot_cost_ore,
                geode_robot_cost_obsidian,
            ) = sscanf!(line, "Blueprint {i32}: Each ore robot costs {i32} ore. Each clay robot costs {i32} ore. Each obsidian robot costs {i32} ore and {i32} clay. Each geode robot costs {i32} ore and {i32} obsidian.").unwrap();
            Blueprint {
                id,
                robot_costs: [
                    (Mineral::Ore, vec![(Mineral::Ore, ore_robot_cost)]),
                    (Mineral::Clay, vec![(Mineral::Ore, clay_robot_cost)]),
                    (Mineral::Obsidian, vec![
                     (Mineral::Ore, obsidian_robot_cost_ore),
                     (Mineral::Clay, obsidian_robot_cost_clay),
                    ]),
                    (Mineral::Geode, vec![
                     (Mineral::Ore, geode_robot_cost_ore),
                     (Mineral::Obsidian, geode_robot_cost_obsidian),
                    ]),
                ].into_iter().collect::<HashMap<_, _>>(),
            }
        })
        .collect::<Vec<_>>();
    //println!("Part 1: {}", part1(&blueprints));
    println!("Part 2: {}", part2(&blueprints));
    Ok(())
}

fn part1(blueprints: &[Blueprint]) -> i32 {
    blueprints.iter().filter_map(|blueprint| {
        let resources = Resources {
            robots: MineralMap {
                ore: 1,
                clay: 0,
                obsidian: 0,
                geode: 0,
            },
            minerals: MineralMap {
                ore: 0,
                clay: 0,
                obsidian: 0,
                geode: 0,
            },
        };
        println!("Blueprint: {}", blueprint.id);
        // Only consider the paths that lead to at least 1 obsidian
        let obsidian_options = part1_first_obsidian(blueprint, resources, 24);
        println!("{} paths to obsidian", obsidian_options.len());

        let best_score = obsidian_options.par_iter().map(|(resources, time)| {
            blueprint.id * part1_solve(blueprint, resources.clone(), *time)
        }).max();
        println!("Best score: {best_score:?}");
        best_score
    }).sum()
}

fn part2(blueprints: &[Blueprint]) -> i32 {
    let mut ans = 1;
    for i in 0..3 {
        let blueprint = &blueprints[i];
        let resources = Resources {
            robots: MineralMap {
                ore: 1,
                clay: 0,
                obsidian: 0,
                geode: 0,
            },
            minerals: MineralMap {
                ore: 0,
                clay: 0,
                obsidian: 0,
                geode: 0,
            },
        };
        println!("Blueprint: {}", blueprint.id);
        // Only consider the paths that lead to at least 1 obsidian
        let obsidian_options = part1_first_obsidian(blueprint, resources, 32);
        println!("{} paths to obsidian", obsidian_options.len());

        let best_score = obsidian_options.par_iter().map(|(resources, time)| {
            part1_solve(blueprint, resources.clone(), *time)
        }).max();
        println!("Best score: {best_score:?}");
        ans *= best_score.unwrap();
    }
    ans
}

fn can_afford(
    blueprint: &Blueprint,
    resources: &Resources,
    mineral: Mineral,
) -> bool {
    let required = blueprint.robot_costs.get(&mineral).unwrap();
    required.iter().all(|(req_mineral, req_count)| {
        *req_count <= resources.minerals.get(*req_mineral)
    })
}

fn try_build_robot(
    blueprint: &Blueprint,
    mut resources: Resources,
    mineral: Mineral,
) -> (Resources, bool) {
    let required = blueprint.robot_costs.get(&mineral).unwrap();
    let can_afford = required.iter().all(|(req_mineral, req_count)| {
        *req_count <= resources.minerals.get(*req_mineral)
    });
    if can_afford {
        for (req_mineral, req_count) in required.iter() {
            resources.minerals.add(*req_mineral, -*req_count);
        }
        (resources, true)
    } else {
        (resources, false)
    }
}

// Find the fastest path to creating an obsidian robot
fn part1_first_obsidian(
    blueprint: &Blueprint,
    resources: Resources,
    time: i32,
) -> Vec<(Resources, i32)> {
    // FIXME: Kinda jank
    let geode_obsidian_cost =
        blueprint.robot_costs.get(&Mineral::Geode).unwrap()[1].1;
    let mut first_obsidians = HashSet::new();
    let mut to_visit: Vec<(Resources, Option<Mineral>, i32)> = vec![(resources, None, time)];

    // Calculate time limit such that past this point, no geode bots could possibly be created
    let mut time_limit = 0;
    for i in 0..time {
        if i * (i + 1) / 2 >= geode_obsidian_cost {
            break;
        }
        time_limit = i;
    }

    println!("First obs time limit: {time_limit}");

    while let Some(next) = to_visit.pop() {
        let new_time = next.2 - 1;
        if new_time == time_limit {
            continue;
        }
        let new_resources = part1_tick(blueprint, next.0, next.1);
        if new_resources.robots.get(Mineral::Obsidian) == 1 {
            first_obsidians.insert((new_resources, new_time));
            continue;
        }

        to_visit.push((new_resources.clone(), None, new_time));
        if can_afford(blueprint, &new_resources, Mineral::Ore) {
            to_visit.push((new_resources.clone(), Some(Mineral::Ore), new_time));
        }
        if can_afford(blueprint, &new_resources, Mineral::Clay) {
            to_visit.push((new_resources.clone(), Some(Mineral::Clay), new_time));
        }
        if can_afford(blueprint, &new_resources, Mineral::Obsidian) {
            to_visit.push((new_resources, Some(Mineral::Obsidian), new_time));
        }
    }

    let mut best_options = HashMap::<MineralMap, Vec<(Resources, i32)>>::new();
    for option in first_obsidians.into_iter() {
        if let Some(best) = best_options.get(&option.0.robots) {
            if option.1 == best[0].1 {
                let mut new_best = best.clone();
                new_best.push(option.clone());
                best_options.insert(option.0.robots, new_best);
            } else if option.1 > best[0].1 {
                best_options.insert(option.0.robots.clone(), vec![option]);
            }
        } else {
            best_options.insert(option.0.robots.clone(), vec![option]);
        }
    }
    best_options.values().flatten().cloned().collect::<Vec<_>>()
}

// Brute force DFS solver
fn part1_solve(
    blueprint: &Blueprint,
    resources: Resources,
    time: i32,
) -> i32 {
    let mut best_score = 0;
    let mut to_visit: Vec<(Resources, Option<Mineral>, i32)> = vec![
        (resources.clone(), None, time),
        (resources.clone(), Some(Mineral::Ore), time),
        (resources.clone(), Some(Mineral::Clay), time),
        (resources.clone(), Some(Mineral::Obsidian), time),
        (resources, Some(Mineral::Geode), time),
    ];
    while let Some(next) = to_visit.pop() {
        if next.2 == 0 {
            best_score = best_score.max(next.0.minerals.get(Mineral::Geode));
            continue;
        }
        let new_time = next.2 - 1;
        let new_resources = part1_tick(blueprint, next.0, next.1);

        if new_resources.minerals.geode + (new_time + 1) * new_time / 2 + new_resources.robots.geode * new_time <= best_score {
            continue;
        }

        // Optimization: Greedily create geode robots
        if can_afford(blueprint, &new_resources, Mineral::Geode) {
            to_visit.push((new_resources, Some(Mineral::Geode), new_time));
        } else {
            if can_afford(blueprint, &new_resources, Mineral::Ore) {
                to_visit.push((new_resources.clone(), Some(Mineral::Ore), new_time));
            }
            if can_afford(blueprint, &new_resources, Mineral::Clay) {
                to_visit.push((new_resources.clone(), Some(Mineral::Clay), new_time));
            }
            if can_afford(blueprint, &new_resources, Mineral::Obsidian) {
                to_visit.push((new_resources.clone(), Some(Mineral::Obsidian), new_time));
            }
            to_visit.push((new_resources, None, new_time));
        }
    }
    best_score
}

fn part1_tick(
    blueprint: &Blueprint,
    resources: Resources,
    spawned_robot: Option<Mineral>,
) -> Resources {
    // Step 1: Optionally attempt to start building a new robot
    let (mut new_resources, should_build) = spawned_robot.map(|mineral| {
        try_build_robot(blueprint, resources.clone(), mineral)
    }).unwrap_or((resources, false));
    // Step 2: mine ore
    for mineral in [Mineral::Obsidian, Mineral::Clay, Mineral::Ore, Mineral::Geode] {
        new_resources.minerals.add(mineral, new_resources.robots.get(mineral));
    }
    // Step 3: add new robots
    if should_build {
        if let Some(robot_mineral) = spawned_robot {
            new_resources.robots.add(robot_mineral, 1);
        }
    }
    new_resources
}
