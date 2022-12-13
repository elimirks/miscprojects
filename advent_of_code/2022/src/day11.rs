use crate::common::*;
use itertools::Itertools;
use sscanf::sscanf;

#[derive(Clone)]
enum Operation {
    Mul(i64),
    Add(i64),
    MulSelf,
    AddSelf,
}

#[derive(Clone)]
struct Monkey {
    items: Vec<i64>,
    operation: Operation,
    test_divisor: i64,
    t_monkey: usize,
    f_monkey: usize,
    inspect_counter: usize,
}

pub fn day11() -> AocResult<()> {
    let input = std::fs::read_to_string("data/day11.txt")?;
    let monkeys = input.split("\n\n").map(parse_chunk).collect::<Vec<_>>();
    println!("Part 1: {}", solve(monkeys.clone(), 20, 3));
    println!("Part 2: {}", solve(monkeys, 10000, 1));
    Ok(())
}

fn solve(mut monkeys: Vec<Monkey>, rounds: usize, divisor: i64) -> usize {
    for _ in 0..rounds {
        step(&mut monkeys, divisor);
    }
    monkeys.iter()
        .map(|monkey| monkey.inspect_counter)
        .sorted().rev()
        .take(2).product()
}

fn step(monkeys: &mut Vec<Monkey>, divisor: i64) {
    // To prevent numbers going out of control
    let lcm = monkeys.iter().map(|m| m.test_divisor).fold(1, num::integer::lcm);
    for i in 0..monkeys.len() {
        let new_items = monkeys[i].items.iter().map(|item|
            match monkeys[i].operation {
                Operation::Mul(rhs) => item * rhs,
                Operation::Add(rhs) => item + rhs,
                Operation::MulSelf  => item * item,
                Operation::AddSelf  => item + item,
            } / divisor % lcm
        ).collect::<Vec<_>>();
        monkeys[i].items.clear();
        monkeys[i].inspect_counter += new_items.len();
        for &new_item in new_items.iter() {
            if new_item % monkeys[i].test_divisor == 0 {
                let t_monkey = monkeys[i].t_monkey;
                monkeys[t_monkey].items.push(new_item);
            } else {
                let f_monkey = monkeys[i].f_monkey;
                monkeys[f_monkey].items.push(new_item);
            }
        }
    }
}

fn parse_chunk(s: &str) -> Monkey {
    let lines = s.lines().collect::<Vec<_>>();
    let items = lines[1].split_once(": ").unwrap().1
        .split(", ")
        .map(|value| value.parse::<i64>().unwrap()).collect::<Vec<_>>();
    let operation =
        if let Ok((op_char, op_value)) = sscanf!(lines[2], "  Operation: new = old {char} {i64}") {
            match op_char {
                '*' => Operation::Mul(op_value),
                '+' => Operation::Add(op_value),
                _   => panic!("Unknown operation"),
            }
        } else {
            match sscanf!(lines[2], "  Operation: new = old {char} old").unwrap() {
                '*' => Operation::MulSelf,
                '+' => Operation::AddSelf,
                _   => panic!("Unknown operation"),
            }
        };
    Monkey {
        items,
        operation,
        test_divisor: sscanf!(lines[3], "  Test: divisible by {i64}").unwrap(),
        t_monkey: sscanf!(lines[4], "    If true: throw to monkey {usize}").unwrap(),
        f_monkey: sscanf!(lines[5], "    If false: throw to monkey {usize}").unwrap(),
        inspect_counter: 0,
    }
}
