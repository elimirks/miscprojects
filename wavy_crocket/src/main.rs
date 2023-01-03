#![feature(box_patterns)]
mod parser;
mod interpreter;
mod sound_handler;
mod math;

use std::{error::Error, process::exit};

// use std::mem::size_of;
// use std::io::prelude::*;
// use std::{f64::consts::PI, error::Error, fs::File};
//
// const HEADER_SIZE: u32 = 44;
// // CD quality. Samples / second
// const SAMPLE_RATE: u32 = 44100;
// // Sample size
// type Sample = i16;
//
// fn amplify(data: &mut Vec<f64>, amount: f64) {
//     for value in data.iter_mut() {
//         *value *= amount;
//     }
// }
//
// fn combine(dest: &mut Vec<f64>, source: &Vec<f64>) {
//     for i in 0..dest.len() {
//         dest[i] += source[i];
//     }
// }
//
// fn decay(data: &mut [f64]) {
//     for i in (0..data.len()).rev() {
//         let x = 1.0 - i as f64 / data.len() as f64;
//         data[i] *= x;
//     }
// }

fn main() -> Result<(), Box<dyn Error>> {
    //unsafe { backtrace_on_stack_overflow::enable() };
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} path/to/generator{{.lisp}}", args[0]);
        exit(1);
    }
    interpreter::run(&args[1])?;
    Ok(())
}
