use crate::common::*;

pub fn day20() -> AocResult<()> {
    let numbers = std::fs::read_to_string("data/day20.txt")?
        .lines()
        .map(|line| line.parse::<i64>().unwrap())
        .collect::<Vec<_>>();
    println!("Part 1: {}", time_closure(|| part1(&numbers)));
    println!("Part 2: {}", time_closure(|| part2(&numbers)));
    Ok(())
}

fn part1(numbers: &[i64]) -> i64 {
    grove_coordinate_sum(&mix(numbers, 1))
}

fn part2(numbers: &[i64]) -> i64 {
    let decrypt_nums = numbers.iter().map(|v| v * 811589153).collect::<Vec<_>>();
    grove_coordinate_sum(&mix(&decrypt_nums, 10))
}

fn mix(numbers: &[i64], mix_num: usize) -> Vec<i64> {
    let mut enumerated = numbers.iter().cloned().enumerate().collect::<Vec<_>>();
    for _ in 0..mix_num {
        (0..enumerated.len()).for_each(|i| mix_index(&mut enumerated, i));
    }
    enumerated.iter().map(|&(_, value)| value).collect::<Vec<_>>()
}

fn mix_index(vec: &mut Vec<(usize, i64)>, index: usize) {
    // FIXME: Kinda wasteful. I don't think we really need to find it every time
    let old_index = vec.iter().position(|&(i, _)| i == index).unwrap();
    let new_index = wrapping_offset(vec.len(), old_index as i64 + vec[old_index].1);
    move_in_vec(vec, old_index, new_index);
}

// Assumes from & to are in bounds of the vec
fn move_in_vec<T>(vec: &mut Vec<T>, from: usize, to: usize) {
    if to < from {
        // Move backward
        vec[to..=from].rotate_right(1);
    } else if to > from {
        // Move forward
        vec[from..=to].rotate_left(1);
    }
}

fn wrapping_offset(size: usize, offset: i64) -> usize {
    offset.rem_euclid(size as i64 - 1) as usize
}

fn grove_coordinate_sum(values: &[i64]) -> i64 {
    let zero_coord = values.iter().position(|&v| v == 0).unwrap();
    values[(zero_coord + 1000) % values.len()]
        + values[(zero_coord + 2000) % values.len()]
        + values[(zero_coord + 3000) % values.len()]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_in_vec() {
        let mut v = vec![1, 2, 3];
        move_in_vec(&mut v, 0, 0);
        assert_eq!(vec![1, 2, 3], v);

        move_in_vec(&mut v, 0, 1);
        assert_eq!(vec![2, 1, 3], v);

        move_in_vec(&mut v, 2, 0);
        assert_eq!(vec![3, 2, 1], v);
    }

    #[test]
    fn test_wrapping_offset() {
        // Test in bounds
        assert_eq!(0, wrapping_offset(5, 0));
        assert_eq!(1, wrapping_offset(5, 1));
        assert_eq!(0, wrapping_offset(5, 4));
        // Test beyond length bound
        // Notice that it goes to 1 instead of 0
        // This is because we treat the "wrap to begin" as a noop
        assert_eq!(1, wrapping_offset(5, 5));
        assert_eq!(2, wrapping_offset(5, 6));
        assert_eq!(3, wrapping_offset(5, 7));
        assert_eq!(0, wrapping_offset(5, 8));
        assert_eq!(1, wrapping_offset(5, 9));
        // Test negative bound
        assert_eq!(4, wrapping_offset(7, 1 - 3));
        // Test negative bound
        assert_eq!(4, wrapping_offset(7, 1 - 3 - 6 * 50));
    }

    #[test]
    fn test_grove_coordinate_sum() {
        assert_eq!(3, grove_coordinate_sum(&vec![1, 2, -3, 4, 0, 3, -2]));
    }

    #[test]
    fn test_mix_index() {
        let mut vec = vec![1, 2, -3, 3, -2, 0, 4]
            .iter().cloned().enumerate().collect::<Vec<(usize, i64)>>();
        mix_index(&mut vec, 0);
        assert_eq!(vec![(1, 2), (0, 1), (2, -3), (3, 3), (4, -2), (5, 0), (6, 4)], vec);
        mix_index(&mut vec, 1);
        assert_eq!(vec![(0, 1), (2, -3), (1, 2), (3, 3), (4, -2), (5, 0), (6, 4)], vec);
        mix_index(&mut vec, 2);
        assert_eq!(vec![(0, 1), (1, 2), (3, 3), (4, -2), (2, -3), (5, 0), (6, 4)], vec);
        mix_index(&mut vec, 3);
        assert_eq!(vec![(0, 1), (1, 2), (4, -2), (2, -3), (5, 0), (3, 3), (6, 4)], vec);
        mix_index(&mut vec, 4);
        assert_eq!(vec![(4, -2), (0, 1), (1, 2),  (2, -3), (5, 0), (3, 3), (6, 4)], vec);
        mix_index(&mut vec, 5);
        assert_eq!(vec![(4, -2), (0, 1), (1, 2),  (2, -3), (5, 0), (3, 3), (6, 4)], vec);
        mix_index(&mut vec, 6);
        assert_eq!(vec![(4, -2), (0, 1), (1, 2),  (2, -3), (6, 4), (5, 0), (3, 3)], vec);
    }

    #[test]
    fn test_mix() {
        assert_eq!(vec![-2, 1, 2, -3, 4, 0, 3], mix(&vec![1, 2, -3, 3, -2, 0, 4], 1));
    }

    #[test]
    fn test_part1() {
        assert_eq!(3, part1(&vec![1, 2, -3, 3, -2, 0, 4]));
    }

    #[test]
    fn test_part2() {
        assert_eq!(1623178306, part2(&vec![1, 2, -3, 3, -2, 0, 4]));
    }
}
