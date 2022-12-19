use std::collections::HashMap;

use crate::common::*;
use sscanf::sscanf;

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
enum Mineral {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

struct Blueprint {
    id: usize,
    // robot type -> cost types
    robot_costs: HashMap<Mineral, Vec<(Mineral, usize)>>,
}

#[derive(Clone)]
struct Resources {
    minerals: HashMap<Mineral, usize>,
    // Robot counts
    robots: HashMap<Mineral, usize>,
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
            ) = sscanf!(line, "Blueprint {usize}: Each ore robot costs {usize} ore. Each clay robot costs {usize} ore. Each obsidian robot costs {usize} ore and {usize} clay. Each geode robot costs {usize} ore and {usize} obsidian.").unwrap();
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
    println!("Part 1: {}", part1(&blueprints));
    Ok(())
}

fn part1(blueprints: &[Blueprint]) -> usize {
    blueprints.iter().map(|blueprint| {
        let resources = Resources {
            robots: [
                (Mineral::Ore, 1),
                (Mineral::Clay, 0),
                (Mineral::Obsidian, 0),
                (Mineral::Geode, 0),
            ].into_iter().collect::<HashMap<_, _>>(),
            minerals: [
                (Mineral::Ore, 0),
                (Mineral::Clay, 0),
                (Mineral::Obsidian, 0),
                (Mineral::Geode, 0),
            ].into_iter().collect::<HashMap<_, _>>(),
        };
        part1_rec(blueprint, &resources, 24)
    }).sum()
}

fn part1_rec(
    blueprint: &Blueprint,
    resources: &Resources,
    time: usize,
) -> usize {
    if time == 0 {
        let quality = resources.minerals.get(&Mineral::Geode).unwrap() * blueprint.id;
        if quality > 5 {
            println!("{quality}");
        }
        return quality;
    }

    // Greedily create geode robots if possible
    {
        let required = blueprint.robot_costs.get(&Mineral::Geode).unwrap();
        let can_afford = required.iter().all(|(req_mineral, req_count)| {
            req_count <= resources.minerals.get(&req_mineral).unwrap()
        });

        if can_afford {
            panic!("{time}");
            let mut new_resources = resources.clone();
            for (req_mineral, req_count) in required.iter() {
                *new_resources.minerals.get_mut(req_mineral).unwrap() -= req_count;
            }
            return part1_spawn(blueprint, &mut new_resources, time, Some(Mineral::Geode));
        }
    };

    // Step 1: optionally start creating a new robot
    let mut best_quality = part1_spawn(blueprint, &mut resources.clone(), time, None);
    for (miner_mineral, required) in blueprint.robot_costs.iter() {
        let can_afford = required.iter().all(|(req_mineral, req_count)| {
            req_count <= resources.minerals.get(&req_mineral).unwrap()
        });
        if can_afford {
            let mut new_resources = resources.clone();
            for (req_mineral, req_count) in required.iter() {
                *new_resources.minerals.get_mut(req_mineral).unwrap() -= req_count;
            }
            best_quality = best_quality.max(
                part1_spawn(blueprint, &mut new_resources, time, Some(*miner_mineral))
            );
        }
    }
    best_quality
}

fn part1_spawn(
    blueprint: &Blueprint,
    resources: &mut Resources,
    time: usize,
    spawned_robot: Option<Mineral>,
) -> usize {
    // Step 2: mine ore
    for (robot_mineral, &robot_count) in resources.robots.iter() {
        *resources.minerals.get_mut(robot_mineral).unwrap() += robot_count;
    }
    // Step 3: add new robots
    if let Some(robot_mineral) = spawned_robot {
        *resources.robots.get_mut(&robot_mineral).unwrap() += 1;
    }
    // Step 4: increment time and repeat
    part1_rec(blueprint, resources, time - 1)
}
