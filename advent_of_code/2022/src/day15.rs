use crate::common::*;
use sscanf::sscanf;

#[derive(Debug)]
struct Scan {
    sensor: Point,
    scan_distance: i64,
}

impl Scan {
    fn new(sensor_x: i64, sensor_y: i64, beacon_x: i64, beacon_y: i64) -> Self {
        let sensor = Point::new(sensor_x, sensor_y);
        let beacon = Point::new(beacon_x, beacon_y);
        Scan {
            sensor,
            scan_distance: sensor.dist(&beacon),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn new(x: i64, y: i64) -> Self {
        Point { x, y }
    }

    fn dist(&self, other: &Point) -> i64 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

#[derive(Clone, Debug)]
struct Range {
    lower: i64,
    upper: i64,
}

impl Range {
    fn new(lower: i64, upper: i64) -> Self {
        assert!(lower <= upper);
        Range { lower, upper }
    }

    fn intersects(&self, other: &Range) -> bool {
        self.upper >= other.lower && self.lower <= other.upper
    }

    fn try_union(&self, other: &Range) -> Option<Range> {
        if self.intersects(other) {
            let lower = self.lower.min(other.lower);
            let upper = self.upper.max(other.upper);
            Some(Range::new(lower, upper))
        } else {
            None
        }
    }

    fn len(&self) -> i64 {
        self.upper - self.lower
    }
}

fn merge_ranges(ranges: &mut Vec<Range>) {
    while try_merge_ranges(ranges) {}
}

fn try_merge_ranges(ranges: &mut Vec<Range>) -> bool {
    for i in 0..ranges.len() {
        for j in (i + 1)..ranges.len() {
            if let Some(combined) = ranges[i].try_union(&ranges[j]) {
                ranges[i] = combined;
                ranges.remove(j);
                return true;
            }
        }
    }
    false
}

pub fn day15() -> AocResult<()> {
    let scans = parse()?;
    println!("Part 1: {}", part1(&scans));
    println!("Part 2: {}", part2(&scans));
    Ok(())
}

fn part1(scans: &[Scan]) -> i64 {
    ranges_scanned_in_row(scans, 2000000).iter()
        .map(|range| range.len()).sum()
}

fn ranges_scanned_in_row(scans: &[Scan], y: i64) -> Vec<Range> {
    let mut ranges = scans.iter().filter_map(|scan| {
        let y_dist = (scan.sensor.y - y).abs();
        let row_x_range = scan.scan_distance - y_dist;

        let lower = scan.sensor.x - row_x_range;
        let upper = scan.sensor.x + row_x_range;

        if upper >= lower {
            Some(Range::new(lower, upper))
        } else {
            None
        }
    }).collect::<Vec<_>>();
    merge_ranges(&mut ranges);
    ranges
}

// Assumes a single unscanned spot that isn't on the edges
fn part2(scans: &[Scan]) -> i64 {
    let limit = 4000000;
    for y in 0..limit {
        let ranges = ranges_scanned_in_row(scans, y);
        if ranges.len() == 2 {
            let x1_upper = ranges[0].upper;
            let x2_lower = ranges[1].lower;
            let x2_upper = ranges[1].upper;

            let missing_x = if x1_upper == x2_lower - 2 {
                x1_upper + 1
            } else {
                x2_upper + 1
            };
            return missing_x * limit + y;
        }
    }
    panic!("Didn't find a solution!")
}

fn parse() -> AocResult<Vec<Scan>> {
    let content = std::fs::read_to_string("data/day15.txt")?;
    Ok(content.lines().map(|line| {
        let (x1, y1, x2, y2) =
            sscanf!(line, "Sensor at x={i64}, y={i64}: closest beacon is at x={i64}, y={i64}")
                .unwrap();
        Scan::new(x1, y1, x2, y2)
    }).collect::<Vec<_>>())
}
