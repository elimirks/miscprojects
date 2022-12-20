use crate::common::*;

pub fn day20() -> AocResult<()> {
    let numbers = std::fs::read_to_string("data/day20.txt")?
        .lines()
        .map(|line| line.parse::<i64>().unwrap())
        .collect::<Vec<_>>();
    println!("Part 1: {}", part1(&numbers));
    Ok(())
}

fn part1(numbers: &[i64]) -> i64 {
    let mixed_numbers = mix(numbers);
    println!("{mixed_numbers:?}");
    0
}

fn mix(numbers: &[i64]) -> Vec<i64> {
    let mut enumerated = numbers.iter().enumerate().collect::<Vec<_>>();
    let mut index = 0;
    let mut i = 0;
    let x = enumerated.iter().map(|&(_, &value)| value).collect::<Vec<_>>();
    println!("Initial arrangement:");
    println!("{x:?}");
    while index < enumerated.len() {
        let move_to_index = {
            let mut x = i as i64 + *enumerated[i].1;
            loop {
                if x < 0 {
                    println!("a");
                    x += enumerated.len() as i64 - 1;
                } else if x >= enumerated.len() as i64 {
                    println!("b");
                    x -= enumerated.len() as i64 + 1;
                } else {
                    break;
                }
            }
            x
        } as usize % enumerated.len();
        println!("{move_to_index}");
        println!("{} moves between {} and {}", enumerated[i].1, enumerated[move_to_index].1, enumerated[move_to_index + 1].1);

        if move_to_index > i {
            // Move forwards
            let mut new_enumerated = vec![];
            new_enumerated.extend_from_slice(&enumerated[..i]);
            new_enumerated.extend_from_slice(&enumerated[i+1..=move_to_index]);
            new_enumerated.push(enumerated[i]);
            new_enumerated.extend_from_slice(&enumerated[move_to_index+1..]);
            enumerated = new_enumerated;
        } else if move_to_index < i {
            // FIXME: Look at the example. Moving -2 to index 0 really moves it to the end of the
            // list!
            println!("broken here!");
            // Move backwards
            let mut new_enumerated = vec![];
            new_enumerated.extend_from_slice(&enumerated[..move_to_index]);
            new_enumerated.push(enumerated[i]);
            new_enumerated.extend_from_slice(&enumerated[move_to_index+1..i]);
            new_enumerated.extend_from_slice(&enumerated[i+1..]);

            enumerated = new_enumerated;
        }
        let x = enumerated.iter().map(|&(_, &value)| value).collect::<Vec<_>>();
        println!("{x:?} after {index}");
        // Move forwards until we hit the proper index
        index += 1;
        i = 0; // _Probably_ not necessary
        while enumerated[i].0 != index {
            i += 1;
        }
    }
    enumerated.iter().map(|&(_, &value)| value).collect::<Vec<_>>()
}
