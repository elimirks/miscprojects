use std::mem::size_of;
use std::io::prelude::*;
use std::{error::Error, fs::File};

const HEADER_SIZE: u32 = 44;
// Sample size
type Sample = i16;

fn to_wav_sample(sample: &[f64]) -> Vec<Sample> {
    sample.iter()
        .map(|value| (value * Sample::MAX as f64) as Sample)
        .collect::<Vec<_>>()
}

// http://soundfile.sapp.org/doc/WaveFormat/
pub fn save_wave(
    sample_rate: u32,
    samples: &[f64],
    file_path: &str,
) -> Result<(), Box<dyn Error>> {
    let data = to_wav_sample(samples);
    let data_size = data.len() * size_of::<Sample>();

    let mut file = File::create(file_path)?;
    // File time marker
    file.write(b"RIFF")?;
    // Entire file size
    file.write(&(data_size as u32 + HEADER_SIZE).to_le_bytes())?;
    // File type header
    file.write(b"WAVE")?;
    // Format chunk marker
    file.write(b"fmt\x20")?;
    // Length of format data
    file.write(&[16, 0, 0, 0])?;
    // Type of format (1 means PCM)
    file.write(&[1, 0])?;
    // Number of channels
    file.write(&[1, 0])?;
    // Sample rate
    file.write(&(sample_rate as u32).to_le_bytes())?;
    // (SampleRate * BitsPerSample * Channels) / 8
    file.write(&((sample_rate * 16 * 1 / 8) as u32).to_le_bytes())?;
    // Block align
    file.write(&(4u16).to_le_bytes())?;
    // Bits per sample
    file.write(&(16u16).to_le_bytes())?;
    // Data chunk header - marks the beginning of the data section
    file.write(b"data")?;
    // Size of data section
    file.write(&(data_size as u32).to_le_bytes())?;
    // File data
    for sample in data.iter() {
        file.write(&sample.to_le_bytes())?;
    }
    Ok(())
}
