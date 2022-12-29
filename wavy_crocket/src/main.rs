#![feature(box_patterns)]
mod parser;
mod interpreter;

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
// fn gen_pure_tone(frequency: f64, wave_count: f64) -> Vec<f64> {
//     let mut data = vec![];
//     let sample_count = wave_count * SAMPLE_RATE as f64 / frequency;
//     for t in 0..sample_count as usize {
//         let x = wave_count * 2.0 * PI * t as f64 / sample_count;
//         data.push(x.sin());
//     }
//     // Hack to get it to go back to zero for the final sample
//     // Really, there should be better normalization logic to avoid drift
//     let last_index = data.len() - 1;
//     data[last_index] = 0.0;
//     data
// }
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
//
// fn to_wav_sample(sample: &Vec<f64>) -> Vec<Sample> {
//     sample.iter()
//         .map(|value| (value * Sample::MAX as f64) as Sample)
//         .collect::<Vec<_>>()
// }
//
// fn gen() -> Vec<f64> {
//     let mut data = vec![];
//     for i in 100..512 {
//         let frequency = i as f64;
//         data.extend_from_slice(&gen_pure_tone(frequency, 1.0));
//     }
//     for i in (100..512).rev() {
//         let frequency = i as f64;
//         data.extend_from_slice(&gen_pure_tone(frequency, 1.0));
//     }
//     amplify(&mut data, 0.1);
//     decay(&mut data);
//     data
// }
//
// // http://soundfile.sapp.org/doc/WaveFormat/
// fn sample_gen() -> Result<(), Box<dyn Error>> {
//     let data = to_wav_sample(&gen());
//     let data_size = data.len() * size_of::<Sample>();
//
//     let mut file = File::create("out.wav")?;
//     // File time marker
//     file.write(b"RIFF")?;
//     // Entire file size
//     file.write(&(data_size as u32 + HEADER_SIZE).to_le_bytes())?;
//     // File type header
//     file.write(b"WAVE")?;
//     // Format chunk marker
//     file.write(b"fmt\x20")?;
//     // Length of format data
//     file.write(&[16, 0, 0, 0])?;
//     // Type of format (1 means PCM)
//     file.write(&[1, 0])?;
//     // Number of channels
//     file.write(&[1, 0])?;
//     // Sample rate
//     file.write(&(SAMPLE_RATE as u32).to_le_bytes())?;
//     // (SampleRate * BitsPerSample * Channels) / 8
//     file.write(&((SAMPLE_RATE * 16 * 1 / 8) as u32).to_le_bytes())?;
//     // Block align
//     file.write(&(4u16).to_le_bytes())?;
//     // Bits per sample
//     file.write(&(16u16).to_le_bytes())?;
//     // Data chunk header - marks the beginning of the data section
//     file.write(b"data")?;
//     // Size of data section
//     file.write(&(data_size as u32).to_le_bytes())?;
//     // File data
//     for sample in data.iter() {
//         file.write(&sample.to_le_bytes())?;
//     }
//     Ok(())
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
