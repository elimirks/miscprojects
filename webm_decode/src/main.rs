use std::{fs::File, io::Read};

pub type AnyRes<T> = anyhow::Result<T>;

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

const UNKNOWN_VINT_LEN: usize = 0xffffffffffffff;

// TODO: Custom parser to more cleanly deal with all this matroska stuff

fn main() -> AnyRes<()> {
    let file = File::open("test.webm")?;
    // Useful guide:
    // https://darkcoding.net/software/reading-mediarecorders-webm-opus-output/
    let bytes = file.bytes()
        .collect::<Result<Vec<_>, _>>()?;
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

    // Parse SimpleBlock elements
    while current_bytes.len() != 0 && current_bytes[0] == 0xa3 {
        println!("SimpleBlock header bytes: {:x?}", &current_bytes[0..16]);
        let (simpleblock_len_size, simpleblock_len) = decode_vint(&current_bytes[1..]);
        println!("SimpleBlock length: {}", simpleblock_len);
        let simpleblock_flags = current_bytes[1 + simpleblock_len_size];
        println!("flags: {:x}", simpleblock_flags);
        let simpleblock_timecode = &current_bytes[1 + simpleblock_len_size + 1..1 + simpleblock_len_size + 3];
        println!("timecode: {:x?}", simpleblock_timecode);
        // let after_simpleblock = &current_bytes[1 + simpleblock_len + simpleblock_len_size..];
        // println!("Next header bytes: {:x?}", &after_simpleblock[0..16]);
        current_bytes = &current_bytes[1 + simpleblock_len + simpleblock_len_size..];
    }
    // Not a terrible issue... maybe warn instead of die?
    assert!(current_bytes.len() == 0);
    println!("{:x?}", current_bytes);
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
