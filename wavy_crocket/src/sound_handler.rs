use std::mem::size_of;
use std::io::prelude::*;
use std::rc::Rc;
use std::{error::Error, fs::File};
use soloud::*;

use gtk::prelude::*;
use gtk::DrawingArea;

use cairo::Context;
use plotters::prelude::*;
use plotters_cairo::CairoBackend;

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

pub fn plot_wavedata(data: Vec<f64>) {
    let application = gtk::Application::new(
        Some("com.mirecki.elijah.wavy_crocket"),
        Default::default(),
    );
    let points = Rc::new(data.clone());
    application.connect_activate(move |app| {
        build_ui(app, points.clone());
    });
    application.run_with_args(&[""]);
}

fn build_ui(app: &gtk::Application, data: Rc<Vec<f64>>) {
    let width = 2048;
    let height = 1024;
    drawable(app, width, height, move |_, cr| {
        let points = data.iter().map(|x| *x).enumerate()
            .collect::<Vec<_>>();

        let root = CairoBackend::new(cr, (width as u32, height as u32)).unwrap().into_drawing_area();

        root.fill(&WHITE).unwrap();
        let root = root.margin(25, 25, 25, 25);

        let mut chart = ChartBuilder::on(&root)
            // Set the caption of the chart
            .caption("Wavy Crocket", ("sans-serif", 40).into_font())
            // Set the size of the label region
            .x_label_area_size(20)
            .y_label_area_size(40)
            // Finally attach a coordinate on the drawing area and make a chart context
            .build_cartesian_2d(0..points.len(), -1f64..1f64)
            .unwrap();

        // Then we can draw a mesh
        chart
            .configure_mesh()
            // We can customize the maximum number of labels allowed for each axis
            .x_labels(5)
            .y_labels(5)
            // We can also change the format of the label text
            .y_label_formatter(&|x| format!("{:.3}", x))
            .draw().unwrap();

        chart.draw_series(LineSeries::new(points, &RED)).unwrap();

        Inhibit(false)
    })
}

fn drawable<F>(application: &gtk::Application, width: i32, height: i32, draw_fn: F)
    where
    F: Fn(&DrawingArea, &Context) -> Inhibit + 'static,
{
    let window = gtk::ApplicationWindow::new(application);
    let drawing_area = Box::new(DrawingArea::new)();

    drawing_area.connect_draw(draw_fn);

    window.set_default_size(width, height);

    window.add(&drawing_area);
    window.show_all();
}
