use std::error::Error;

type MlResult<T> = Result<T, Box<dyn Error>>;

// See here for file formats
// http://yann.lecun.com/exdb/mnist/
fn load_mnist_labels(path: &str) -> MlResult<Vec<u8>> {
    let mut bytes = std::fs::read(path)?;
    // Ignore magic header and file length
    bytes.drain(0..8);
    Ok(bytes)
}

// Returns a list of "flattened" images from an mnist image file
// "Pixel" values range from 0.0-1.0
fn load_mnist_images(path: &str) -> MlResult<Vec<Vec<f64>>> {
    let bytes = std::fs::read(path)?;
    let image_count = u32::from_be_bytes(bytes[4..8].try_into()?) as usize;
    let row_count = u32::from_be_bytes(bytes[8..12].try_into()?);
    let col_count = u32::from_be_bytes(bytes[12..16].try_into()?);
    let image_size = (row_count * col_count) as usize;
    let mut images = vec![];
    let data_offset = 16;
    for i in 0..image_count {
        let image_offset = i * image_size + data_offset;
        let mut image = vec![];
        for j in 0..image_size {
            image.push(bytes[j + image_offset] as f64 / 255.0);
        }
        images.push(image);
    }
    Ok(images)
}

fn main() -> MlResult<()> {
    println!("Hello, world!");
    let images = load_mnist_images("./data/train-images-idx3-ubyte")?;
    let labels = load_mnist_labels("./data/train-labels-idx1-ubyte")?;
    println!("Loaded {} images and {} labels", images.len(), labels.len());
    Ok(())
}
