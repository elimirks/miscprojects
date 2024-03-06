use std::{collections::VecDeque, fs::File, io::Read, pin::Pin, sync::{Arc, Mutex}, task::Poll};

use opus::Decoder;
use futures::{Stream, StreamExt};

use anyhow::bail;
pub type AnyRes<T> = anyhow::Result<T>;

const BITRATE: i32 = 48_000;
const UNKNOWN_VINT_LEN: usize = 0xffffffffffffff;

struct ByteStream {
    inner: Arc<Mutex<ByteStreamInner>>,
}

struct ByteStreamInner {
    bytes: VecDeque<u8>,
    is_closed: bool,
}

impl ByteStream {
    fn new() -> Self {
        ByteStream {
            inner: Arc::new(Mutex::new(ByteStreamInner {
                bytes: VecDeque::new(),
                is_closed: false,
            })),
        }
    }

    fn enqueue_chunk(&self, chunk: Vec<u8>) -> AnyRes<()> {
        let mut inner =
            self.inner.lock().expect("Failed locking byte stream");
        if inner.is_closed {
            bail!("Tried queuing to ForwardingStream while it was closed");
        }
        inner.bytes.extend(chunk.iter());
        Ok(())
    }

    fn dequeue_byte(&self) -> Option<u8> {
        let mut inner =
            self.inner.lock().expect("Failed locking byte stream");
        inner.bytes.pop_front()
    }

    fn close(&self) {
        let mut inner =
            self.inner.lock().expect("Failed locking byte stream");
        inner.is_closed = true;
    }

    fn is_closed(&self) -> bool {
        let inner =
            self.inner.lock().expect("Failed locking forwarding stream");
        inner.is_closed
    }

    fn poll_next_n<const N: usize>(
        &self,
        _cx: &mut std::task::Context<'_>,
    ) -> Poll<Option<[u8; N]>> {
        let mut inner =
            self.inner.lock().expect("Failed locking byte stream");
        if inner.bytes.len() < N {
            if inner.is_closed {
                Poll::Ready(None)
            } else {
                Poll::Pending
            }
        } else {
            let mut res = [0; N];
            let mut i = 0;
            while i < res.len() {
                res[i] = inner.bytes.pop_front().unwrap();
                i += 1;
            }
            Poll::Ready(Some(res))
        }
    }

    fn try_dequeue_vint(&self) -> Option<usize> {
        let mut inner =
            self.inner.lock().expect("Failed locking byte stream");
        if inner.bytes.is_empty() {
            None
        } else {
            let octet_count = 1 + inner.bytes[0].leading_zeros() as usize;
            if inner.bytes.len() < octet_count {
                None
            } else {
                let mut octets: Vec<u8> = Vec::with_capacity(octet_count);
                for _ in 0..octet_count {
                    octets.push(inner.bytes.pop_front().unwrap());
                }
                let marker_pos = 8 - octets[0].leading_zeros() as u8;
                if marker_pos != 0 {
                    let anti_marker_mask = !(1 << (marker_pos - 1)) as u8;
                    octets[0] &= anti_marker_mask;
                }
                let mut vint = 0;
                for (i, &octet) in octets.iter().enumerate() {
                    vint |= (octet as usize) << (8 * (octets.len() - i - 1));
                }
                Some(vint)
            }
        }
    }
/*
fn decode_vint(bytes: &[u8]) -> (usize, usize) {
    assert!(bytes.len() >= 1);
    let octet_count = 1 + bytes[0].leading_zeros() as usize;
    let mut octets: Vec<u8> = bytes[0..octet_count].iter().copied().collect();
    let marker_pos = 8 - bytes[0].leading_zeros() as u8;
    if marker_pos != 0 {
        let anti_marker_mask = !(1 << (marker_pos - 1)) as u8;
        octets[0] &= anti_marker_mask;
    }
    let mut vint = 0;
    for (i, &octet) in octets.iter().enumerate() {
        vint |= (octet as usize) << (8 * (octets.len() - i - 1));
    }
    (octet_count, vint)
}
*/
}

impl Stream for ByteStream {
    type Item = u8;

    fn poll_next(
        self: Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> Poll<Option<Self::Item>> {
        let maybe_byte = self.dequeue_byte();
        if let Some(byte) = maybe_byte {
            Poll::Ready(Some(byte))
        } else {
            if self.is_closed() {
                Poll::Ready(None)
            } else {
                Poll::Pending
            }
        }
    }
}

#[derive(Debug)]
enum EmblElement {
    WebmHeader,
    Segment,
    SegmentInfo,
    Tracks,
    Cluster,
    Timecode,
    SimpleBlock {
        data: Vec<u8>,
    },
    Unexpected,
}

struct EmblStream {
    inner: Arc<Mutex<EmblStreamInner>>,
}

impl EmblStream {
    fn new() -> Self {
        EmblStream {
            inner: Arc::new(Mutex::new(EmblStreamInner {
                bytes: VecDeque::new(),
                is_closed: false,
            })),
        }
    }

    fn enqueue_chunk(&self, chunk: Vec<u8>) -> AnyRes<()> {
        let mut inner =
            self.inner.lock().expect("Failed locking byte stream");
        if inner.is_closed {
            bail!("Tried queuing to ForwardingStream while it was closed");
        }
        inner.bytes.extend(chunk.iter());
        Ok(())
    }

    fn close(&self) {
        let mut inner =
            self.inner.lock().expect("Failed locking byte stream");
        inner.is_closed = true;
    }
}

struct EmblStreamInner {
    bytes: VecDeque<u8>,
    is_closed: bool,
}

impl EmblStreamInner {
    // Somewhat similar to a regular Stream::poll_next, except it returns a None if
    // the stream is out of bytes to return, regardless of if it's closed or not.
    fn poll_next(
        &mut self,
    ) -> Option<EmblElement> {
        // To let us treat the VecDeque as a slice more easily
        // self.bytes.make_contiguous();
        match self.bytes.get(0) {
            Some(0x1a) => {
                self.try_skipping_element(
                    EmblElement::WebmHeader,
                    &[0x1a, 0x45, 0xdf, 0xa3],
                )
            }
            Some(0x18) => {
                self.try_skipping_element(
                    EmblElement::Segment,
                    &[0x18, 0x53, 0x80, 0x67],
                )
            }
            Some(0x15) => {
                self.try_skipping_element(
                    EmblElement::SegmentInfo,
                    &[0x15, 0x49, 0xa9, 0x66],
                )
            }
            Some(0x16) => {
                self.try_skipping_element(
                    EmblElement::Tracks,
                    &[0x16, 0x54, 0xae, 0x6b],
                )
            }
            Some(0x1f) => {
                self.try_skipping_element(
                    EmblElement::Cluster,
                    &[0x1f, 0x43, 0xb6, 0x75],
                )
            }
            Some(0xe7) => {
                self.try_skipping_element(
                    EmblElement::Timecode,
                    &[0xe7],
                )
            }
            Some(0xa3) => {
                todo!("Implement reading the SimpleBlock into a new vec")
            }
            Some(_) => {
                Some(EmblElement::Unexpected)
            },
            None => None
        }
    }

    /// Returns None if we hit the end of the buffer
    /// Will only skip the element if we have the entire element in the buffer.
    /// So if for some reason an element gets cut in half across the
    /// websocket stream, it will still work fine.
    fn try_skipping_element(
        &mut self,
        element: EmblElement,
        id: &[u8],
    ) -> Option<EmblElement> {
        if self.bytes.len() < id.len() {
            return None;
        }
        for i in 0..id.len() {
            if self.bytes.get(i) != Some(&id[i]) {
                return Some(EmblElement::Unexpected);
            }
        }
        let offset = id.len();
        // vint decoding for element size
        let Some(first_octet) = self.bytes.get(offset) else {
            return None;
        };
        let octet_count = 1 + first_octet.leading_zeros() as usize;
        let mut octets = [0; 8];
        for i in 0..octet_count {
            if let Some(&octet) = self.bytes.get(offset + i) {
                octets[i] = octet;
            } else {
                return None;
            }
        }
        let marker_pos = 8 - octets[0].leading_zeros() as u8;
        if marker_pos != 0 {
            let anti_marker_mask = !(1 << (marker_pos - 1)) as u8;
            octets[0] &= anti_marker_mask;
        }
        let mut element_size = 0;
        for i in 0..octet_count {
            element_size |= (octets[i] as usize) << (8 * (octet_count - i - 1));
        }
        if element_size == UNKNOWN_VINT_LEN {
            self.bytes.drain(0..id.len() + octet_count);
        } else {
            self.bytes.drain(0..id.len() + octet_count + element_size);
        }
        Some(element)
    }
}

impl Stream for EmblStream {
    type Item = EmblElement;

    fn poll_next(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Option<Self::Item>> {
        let mut inner = self.inner.lock().expect("Failed locking embl stream");
        if let Some(element) = inner.poll_next() {
            Poll::Ready(Some(element))
        } else {
            if inner.is_closed {
                Poll::Ready(None)
            } else {
                Poll::Pending
            }
        }
    }
}

// NOTE: VINTs in EMBL are serialized as big-endian ints
// https://github.com/ietf-wg-cellar/ebml-specification/blob/master/specification.markdown#vint-examples
/// Decodes a variable-length integer from the given bytes.
/// Returns the length of the vint and the decoded integer.
fn decode_vint(bytes: &[u8]) -> (usize, usize) {
    assert!(bytes.len() >= 1);
    let octet_count = 1 + bytes[0].leading_zeros() as usize;
    let mut octets: Vec<u8> = bytes[0..octet_count].iter().copied().collect();
    let marker_pos = 8 - bytes[0].leading_zeros() as u8;
    if marker_pos != 0 {
        let anti_marker_mask = !(1 << (marker_pos - 1)) as u8;
        octets[0] &= anti_marker_mask;
    }
    let mut vint = 0;
    for (i, &octet) in octets.iter().enumerate() {
        vint |= (octet as usize) << (8 * (octets.len() - i - 1));
    }
    (octet_count, vint)
}

// TODO: Custom parser to more cleanly deal with all this matroska stuff

fn strip_ogg_from_webm(bytes: &[u8]) -> AnyRes<Vec<Vec<u8>>> {
    // Check for webm magic number
    assert!(bytes[0..4] == [0x1a, 0x45, 0xdf, 0xa3]);
    let (embl_len_size, embl_len) = decode_vint(&bytes[4..]);
    let matroska_header_index = 4 + embl_len_size + embl_len;
    // Skip past the EMBL header to go to the Matroska header
    let matroska_start = &bytes[matroska_header_index..];
    // Expect a Matroska segment ID
    // https://www.ietf.org/archive/id/draft-lhomme-cellar-matroska-04.txt
    assert!(matroska_start[0..4] == [0x18, 0x53, 0x80, 0x67]);
    let (segment_len_size, segment_len) = decode_vint(&matroska_start[4..]);
    assert!(segment_len == UNKNOWN_VINT_LEN);
    let after_segment = &matroska_start[4 + segment_len_size..];
    // Expect segment info header
    assert!(after_segment[0..4] == [0x15, 0x49, 0xa9, 0x66]);
    let (segment_info_len_size, segment_info_len) = decode_vint(&after_segment[4..]);
    let after_segment_info = &after_segment[4 + segment_info_len_size + segment_info_len..];
    // Tracks header
    assert!(after_segment_info[0..4] == [0x16, 0x54, 0xae, 0x6b]);
    let (tracks_len_size, tracks_len) = decode_vint(&after_segment_info[4..]);
    let after_tracks = &after_segment_info[4 + tracks_len_size + tracks_len..];
    // Cluster header
    assert!(after_tracks[0..4] == [0x1f, 0x43, 0xb6, 0x75]);
    let (cluster_len_size, cluster_len) = decode_vint(&after_tracks[4..]);
    assert!(cluster_len == UNKNOWN_VINT_LEN);
    let after_cluster = &after_tracks[4 + cluster_len_size..];
    // Timecode header. Expect to be at value 0 since we're at the start of the stream
    assert!(after_cluster[0..3] == [0xe7, 0x81, 0x0]);
    let after_timecode = &after_cluster[3..];

    let mut current_bytes = after_timecode;

    // TODO: Try a longer sample, see if it has additional cluster headers after
    // the SimpleBlock timecodes hit 0xff

    let mut ogg_packets = vec![];
    // Parse SimpleBlock elements
    while current_bytes.len() != 0 && current_bytes[0] == 0xa3 {
        println!("SimpleBlock header bytes: {:x?}", &current_bytes[0..16]);
        let (simpleblock_len_size, simpleblock_len) = decode_vint(&current_bytes[1..]);
        println!("SimpleBlock length: {}", simpleblock_len);
        let track_num = current_bytes[1 + simpleblock_len_size];
        println!("track: {:x}", track_num);
        let simpleblock_timecode = &current_bytes[1 + simpleblock_len_size + 1..1 + simpleblock_len_size + 3];
        println!("timecode: {:x?}", simpleblock_timecode);
        let track_num = current_bytes[1 + simpleblock_len_size];
        println!("flags: {:x}", current_bytes[1 + simpleblock_len_size + 3]);

        let next_segment_index = 1 + simpleblock_len + simpleblock_len_size;
        let block_data = &current_bytes[1 + simpleblock_len_size + 4..next_segment_index];
        println!("data len: {}", block_data.len());
        ogg_packets.push(block_data.to_vec());
        // let after_simpleblock = &current_bytes[1 + simpleblock_len + simpleblock_len_size..];
        // println!("Next header bytes: {:x?}", &after_simpleblock[0..16]);
        current_bytes = &current_bytes[next_segment_index..];
    }
    // Not a terrible issue... maybe warn instead of die?
    assert!(current_bytes.len() == 0);
    println!("{:x?}", current_bytes);
    Ok(ogg_packets)
}

#[tokio::main]
async fn main() -> AnyRes<()> {
    let file = File::open("test.webm")?;
    // Useful guide:
    // https://darkcoding.net/software/reading-mediarecorders-webm-opus-output/
    let bytes = file.bytes()
        .collect::<Result<Vec<_>, _>>()?;
    let mut embl_stream = EmblStream::new();
    embl_stream.enqueue_chunk(bytes)?;
    embl_stream.close();
    println!("{:?}", embl_stream.next().await);
    println!("{:?}", embl_stream.next().await);
    println!("{:?}", embl_stream.next().await);
    println!("{:?}", embl_stream.next().await);
    println!("{:?}", embl_stream.next().await);
    println!("{:?}", embl_stream.next().await);
    println!("{:?}", embl_stream.next().await);
    println!("{:?}", embl_stream.next().await);
    println!("{:?}", embl_stream.next().await);

    // let mut counter = 0;
    // while let Some(_) = embl_stream.next().await {
    //     counter += 1;
    // }
    // println!("And {counter} more elements");
    /*
    let ogg_packets = strip_ogg_from_webm(&bytes)?;
    let mut decoder = Decoder::new(BITRATE as u32, opus::Channels::Mono)?;
    let mut pcm = vec![];
    for packet in ogg_packets {
        pcm.resize(decoder.get_nb_samples(&packet)?, 0);
        decoder.decode(&packet, pcm.as_mut_slice(), false)?;
    }
    let mut pcm_bytes = vec![];
    for sample in pcm {
        pcm_bytes.extend_from_slice(&sample.to_le_bytes());
    }
    */
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode_vint() -> AnyRes<()> {
        assert_eq!(decode_vint(&[0b10000000, 0xff]), (1, 0));
        assert_eq!(decode_vint(&[0b10000001, 0xff]), (1, 1));
        assert_eq!(decode_vint(&[0b10010001, 0xff]), (1, 0b00010001));
        assert_eq!(decode_vint(&[0b11010001, 0xff]), (1, 0b01010001));
        assert_eq!(decode_vint(&[0b11010001]), (1, 0b01010001));
        assert_eq!(decode_vint(&[0b11010001]), (1, 0b01010001));
        assert_eq!(decode_vint(&[0b10011111]), (1, 31));
        assert_eq!(decode_vint(&[0b01000000, 0b00011111]), (2, 31));
        assert_eq!(decode_vint(&[0b01000000, 0b00000000]), (2, 0));
        assert_eq!(decode_vint(&[0b01010000, 0b00000000]), (2, 4096));
        assert_eq!(decode_vint(&[0b01100000, 0b00000000]), (2, 8192));
        assert_eq!(decode_vint(&[0b01100000, 0b00000001]), (2, 8193));
        assert_eq!(decode_vint(&[
            0b00000001,
            0b00000000,
            0b00000000,
            0b00000000,
            0b00000000,
            0b00000001,
            0b00000000,
            0b00000001,
        ]), (8, 65537));
        Ok(())
    }
}
