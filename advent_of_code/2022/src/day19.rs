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
    ore: i16,
    clay: i16,
    obsidian: i16,
    geode: i16,
}

impl MineralMap {
    fn get(&self, mineral: Mineral) -> i16 {
        match mineral {
            Mineral::Ore => self.ore,
            Mineral::Clay => self.clay,
            Mineral::Obsidian => self.obsidian,
            Mineral::Geode => self.geode,
        }
    }

    fn add(&mut self, mineral: Mineral, to_add: i16) {
        match mineral {
            Mineral::Ore => self.ore += to_add,
            Mineral::Clay => self.clay += to_add,
            Mineral::Obsidian => self.obsidian += to_add,
            Mineral::Geode => self.geode += to_add,
        }
    }

    fn sub_from_mm(&mut self, other: &MineralMap) {
        self.ore -= other.ore;
        self.clay -= other.clay;
        self.obsidian -= other.obsidian;
        self.geode -= other.geode;
    }
}

struct Blueprint {
    id: i16,
    robot_cost_ore: MineralMap,
    robot_cost_clay: MineralMap,
    robot_cost_obsidian: MineralMap,
    robot_cost_geode: MineralMap,
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
            ) = sscanf!(line, "Blueprint {i16}: Each ore robot costs {i16} ore. Each clay robot costs {i16} ore. Each obsidian robot costs {i16} ore and {i16} clay. Each geode robot costs {i16} ore and {i16} obsidian.").unwrap();
            Blueprint {
                id,
                robot_cost_ore: MineralMap {
                    ore: ore_robot_cost,
                    clay: 0, obsidian: 0, geode: 0
                },
                robot_cost_clay: MineralMap {
                    ore: clay_robot_cost,
                    clay: 0, obsidian: 0, geode: 0
                },
                robot_cost_obsidian: MineralMap {
                    ore: obsidian_robot_cost_ore,
                    clay: obsidian_robot_cost_clay,
                    obsidian: 0,
                    geode: 0
                },
                robot_cost_geode: MineralMap {
                    ore: geode_robot_cost_ore,
                    clay: 0,
                    obsidian: geode_robot_cost_obsidian,
                    geode: 0
                },
            }
        })
        .collect::<Vec<_>>();
    println!("Part 1: {}", part1(&blueprints));
    println!("Part 2: {}", part2(&blueprints));
    Ok(())
}

fn part1(blueprints: &[Blueprint]) -> i16 {
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

fn part2(blueprints: &[Blueprint]) -> i16 {
    let mut ans = 1;
    for blueprint in blueprints.iter().take(3) {
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
    let required = match mineral {
        Mineral::Ore      => &blueprint.robot_cost_ore,
        Mineral::Clay     => &blueprint.robot_cost_clay,
        Mineral::Obsidian => &blueprint.robot_cost_obsidian,
        Mineral::Geode    => &blueprint.robot_cost_geode,
    };
    // Don't bother checking geode cost, it's never used
    resources.minerals.ore >= required.ore &&
        resources.minerals.clay >= required.clay &&
        resources.minerals.obsidian >= required.obsidian
}

// Find the fastest path to creating an obsidian robot
fn part1_first_obsidian(
    blueprint: &Blueprint,
    resources: Resources,
    time: i16,
) -> Vec<(Resources, i16)> {
    let geode_obsidian_cost = blueprint.robot_cost_geode.obsidian;
    let mut first_obsidians = HashSet::new();
    let mut to_visit: Vec<(Resources, Option<Mineral>, i16)> = Vec::with_capacity(1000);
    to_visit.push((resources, None, time));

    // Calculate time limit such that past this point, no geode bots could possibly be created
    let mut time_limit = 0;
    for i in 0..time {
        if i * (i + 1) / 2 >= geode_obsidian_cost {
            break;
        }
        time_limit = i;
    }
    let max_build_delay = 2;
    let mut earliest_time = time_limit;

    println!("First obs time limit: {time_limit}");

    while let Some(next) = to_visit.pop() {
        let new_time = next.2 - 1;
        if new_time == time_limit {
            continue;
        }
        if new_time + max_build_delay < earliest_time {
            continue;
        }
        let new_resources = part1_tick(blueprint, next.0, next.1);
        if new_resources.robots.get(Mineral::Obsidian) == 1 {
            earliest_time = earliest_time.max(new_time);
            first_obsidians.insert((new_resources, new_time));
            continue;
        }

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

    let mut best_options = HashMap::<MineralMap, Vec<(Resources, i16)>>::new();
    for option in first_obsidians.into_iter() {
        if option.1 + max_build_delay < earliest_time {
            continue;
        }
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
    time: i16,
) -> i16 {
    let mut best_score = 0;
    let mut to_visit: Vec<(Resources, Option<Mineral>, i16)> = Vec::with_capacity(1000);

    if can_afford(blueprint, &resources, Mineral::Ore) {
        to_visit.push((resources.clone(), Some(Mineral::Ore), time));
    }
    if can_afford(blueprint, &resources, Mineral::Clay) {
        to_visit.push((resources.clone(), Some(Mineral::Clay), time));
    }
    if can_afford(blueprint, &resources, Mineral::Obsidian) {
        to_visit.push((resources.clone(), Some(Mineral::Clay), time));
    }
    if can_afford(blueprint, &resources, Mineral::Geode) {
        to_visit.push((resources.clone(), Some(Mineral::Geode), time));
    }
    to_visit.push((resources, None, time));

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

// Assumes there are enough resources to spawn the given robot
fn part1_tick(
    blueprint: &Blueprint,
    mut resources: Resources,
    spawned_robot: Option<Mineral>,
) -> Resources {
    // Step 1: Take away resources for teh given robot type
    if let Some(mineral) = spawned_robot {
        resources.minerals.sub_from_mm(match mineral {
            Mineral::Ore => &blueprint.robot_cost_ore,
            Mineral::Clay => &blueprint.robot_cost_clay,
            Mineral::Obsidian => &blueprint.robot_cost_obsidian,
            Mineral::Geode => &blueprint.robot_cost_geode,
        });
    }
    // Step 2: mine ore
    for mineral in [Mineral::Obsidian, Mineral::Clay, Mineral::Ore, Mineral::Geode] {
        resources.minerals.add(mineral, resources.robots.get(mineral));
    }
    // Step 3: add new robots
    if let Some(robot_mineral) = spawned_robot {
        resources.robots.add(robot_mineral, 1);
    }
    resources
}
