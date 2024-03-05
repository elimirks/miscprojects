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

fn main() -> AnyRes<()> {
    let file = File::open("test.webm")?;
    let bytes = file.bytes()
        .collect::<Result<Vec<_>, _>>()?;
    // Check for webm magic number
    assert!(bytes[0..4] == [0x1a, 0x45, 0xdf, 0xa3]);
    let (embl_len_size, embl_len) = decode_vint(&bytes[4..]);
    println!("EMBL len: {}", embl_len);
    let matroska_header_index = 4 + embl_len_size + embl_len;
    let matroska_start = &bytes[matroska_header_index..];
    println!("{:02X?}", &matroska_start[0..4]);
    //println!("SimpleBlock len: {}", simpleblock_len(matroska_start));
    println!("Hello, world!");
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
