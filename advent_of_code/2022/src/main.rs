mod common;
mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day11;

fn main() -> common::AocResult<()> {
    let days = [
        day01::day01,
        day02::day02,
        day03::day03,
        day04::day04,
        day05::day05,
        day06::day06,
        day07::day07,
        day08::day08,
        day09::day09,
        day09::day09,
        day11::day11,
    ];
    let day_num = std::env::args()
        .nth(1).expect("Day num is required")
        .parse::<usize>().expect("Given day num is not an integer");
    if day_num < 1 || day_num > days.len() {
        panic!("Invalid day num");
    }
    let func = days[day_num - 1];
    func()
}
