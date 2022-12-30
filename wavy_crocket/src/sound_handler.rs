use std::mem::size_of;
use std::io::prelude::*;
use std::{error::Error, fs::File};
use soloud::*;

const HEADER_SIZE: u32 = 44;
// Sample size
type Sample = i16;

fn to_wav_sample(sample: &[f64]) -> Vec<Sample> {
    sample.iter()
        .map(|value| (value * Sample::MAX as f64) as Sample)
        .collect::<Vec<_>>()
}

pub fn save_wave(
    sample_rate: u32,
    samples: &[f64],
    file_path: &str,
) -> Result<(), Box<dyn Error>> {
    let mut file = File::create(file_path)?;
    file.write_all(&gen_wave_file(sample_rate, samples))?;
    Ok(())
}

pub fn play_wave(
    sample_rate: u32,
    samples: &[f64],
) -> Result<(), Box<dyn Error>> {
    let raw = gen_wave_file(sample_rate, samples);
    let sl = Soloud::default()?;
    let mut wav = Wav::default();
    wav.load_mem(&raw)?;
    // For some reason, there is artifacting if you play too soon
    std::thread::sleep(std::time::Duration::from_millis(100));
    sl.play(&wav);
    while sl.voice_count() > 0 {
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    Ok(())
}

// http://soundfile.sapp.org/doc/WaveFormat/
fn gen_wave_file(
    sample_rate: u32,
    samples: &[f64],
) -> Vec<u8> {
    let data = to_wav_sample(samples);
    let data_size = data.len() * size_of::<Sample>();

    let mut raw = vec![];
    // File time marker
    raw.extend_from_slice(b"RIFF");
    // Entire file size
    raw.extend_from_slice(&(data_size as u32 + HEADER_SIZE).to_le_bytes());
    // File type header
    raw.extend_from_slice(b"WAVE");
    // Format chunk marker
    raw.extend_from_slice(b"fmt\x20");
    // Length of format data
    raw.extend_from_slice(&[16, 0, 0, 0]);
    // Type of format (1 means PCM)
    raw.extend_from_slice(&[1, 0]);
    // Number of channels
    raw.extend_from_slice(&[1, 0]);
    // Sample rate
    raw.extend_from_slice(&(sample_rate as u32).to_le_bytes());
    // (SampleRate * BitsPerSample * Channels) / 8
    raw.extend_from_slice(&((sample_rate * 16 * 1 / 8) as u32).to_le_bytes());
    // Block align
    raw.extend_from_slice(&(4u16).to_le_bytes());
    // Bits per sample
    raw.extend_from_slice(&(16u16).to_le_bytes());
    // Data chunk header - marks the beginning of the data section
    raw.extend_from_slice(b"data");
    // Size of data section
    raw.extend_from_slice(&(data_size as u32).to_le_bytes());
    // File data
    for sample in data.iter() {
        raw.extend_from_slice(&sample.to_le_bytes());
    }
    raw
}
