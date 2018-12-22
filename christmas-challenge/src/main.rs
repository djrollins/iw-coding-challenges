use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::f64::{INFINITY, MIN, NEG_INFINITY};
use std::hash::{Hash, Hasher};
use std::io::{self, Read};
use std::ops::Deref;
use std::str::FromStr;

use rayon::prelude::*;

type Id = u32;

// NotNan wrappers are to so I can create Eq and Ord implementations so that
// Records can be put in associative containers. floating point numbers aren't
// are only PartialOrd and PartialEq by default because NaN != NaN
#[derive(Debug, Clone, PartialEq)]
struct Record {
    id: Id,
    lat: NotNan,
    lon: NotNan,
}

// Just hash the id
impl Hash for Record {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl Eq for Record {}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
struct NotNan {
    as_f64: f64,
}

impl Deref for NotNan {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.as_f64
    }
}

impl FromStr for NotNan {
    type Err = Box<Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        NotNan::from_f64(s.parse()?).ok_or_else(|| Box::<Error>::from("was NaN"))
    }
}

impl NotNan {
    fn from_f64(as_f64: f64) -> Option<Self> {
        if as_f64.is_nan() {
            None
        } else {
            Some(NotNan { as_f64 })
        }
    }

    fn infinity() -> Self {
        NotNan::from_f64(INFINITY).unwrap()
    }

    fn neg_infinity() -> Self {
        NotNan::from_f64(NEG_INFINITY).unwrap()
    }

    fn min() -> Self {
        NotNan::from_f64(MIN).unwrap()
    }
}

impl Eq for NotNan {}

impl Ord for NotNan {
    fn cmp(&self, other: &NotNan) -> Ordering {
        match self.partial_cmp(other) {
            Some(ordering) => ordering,
            None => unreachable!(),
        }
    }
}

fn hav_distance((lat1, lon1): (NotNan, NotNan), (lat2, lon2): (NotNan, NotNan)) -> NotNan {
    let lat1 = *lat1;
    let lat2 = *lat2;
    let lon1 = *lon1;
    let lon2 = *lon2;

    let r = 6372.8;
    let d_lat = (lat2 - lat1).to_radians();
    let d_lon = (lon2 - lon1).to_radians();
    let lat1 = lat1.to_radians();
    let lat2 = lat2.to_radians();

    let x = (d_lat / 2f64).sin().powi(2) + lat1.cos() * lat2.cos() * (d_lon / 2f64).sin().powi(2);

    NotNan::from_f64(x.sqrt().asin() * 2f64 * r).unwrap()
}

fn read_input() -> Result<Vec<Record>, Box<Error>> {
    let mut input = String::new();
    let mut records = Vec::new();

    io::stdin().read_to_string(&mut input)?;

    for line in input.lines().skip(1) {
        let values: Vec<_> = line.split('\t').collect();
        records.push(Record {
            id: values[0].parse()?,
            lat: values[2].parse()?,
            lon: values[3].parse()?,
        });
    }

    Ok(records)
}

fn solve_by_nearest(records: &[Record]) -> Vec<Id> {
    let mut records: HashSet<Record> = records.iter().cloned().collect();
    let mut coord = (NotNan::min(), NotNan::min());
    let mut result = Vec::new();

    loop {
        let nearest = records.iter().min_by(|x, y| {
            let x_dist = hav_distance(coord, (x.lat, x.lon));
            let y_dist = hav_distance(coord, (y.lat, y.lon));

            x_dist.cmp(&y_dist)
        });

        let nearest = match nearest {
            None => return result,
            Some(nearest) => nearest.clone(),
        };

        let nearest = records.take(&nearest).unwrap();
        result.push(nearest.id);
        coord = (nearest.lat, nearest.lon);
    }
}

const GRID_SIZE: usize = 4;

fn chunk(coords: Vec<Record>) -> Vec<Vec<Record>> {
    let min_max = (
        (NotNan::infinity(), NotNan::infinity()),
        (NotNan::neg_infinity(), NotNan::neg_infinity()),
    );

    let ((min_lat, min_lon), (max_lat, max_lon)) =
        coords.iter().fold(min_max, |(min, max), coord| {
            (
                (
                    std::cmp::min(coord.lat, min.0),
                    std::cmp::min(coord.lon, min.1),
                ),
                (
                    std::cmp::max(coord.lat, max.0),
                    std::cmp::max(coord.lon, max.1),
                ),
            )
        });

    let max_lat_norm = *max_lat - *min_lat;
    let max_lon_norm = *max_lon - *min_lon;

    coords.into_iter().fold(
        vec![Vec::new(); GRID_SIZE * GRID_SIZE],
        |mut grid, record| {
            let lat_norm = *record.lat - *min_lat;
            let lon_norm = *record.lon - *min_lon;
            let lat_bucket = ((GRID_SIZE as f64) * (lat_norm / max_lat_norm)).floor() as usize;
            let lon_bucket = ((GRID_SIZE as f64) * (lon_norm / max_lon_norm)).floor() as usize;
            grid[lat_bucket * lon_bucket].push(record);
            grid
        },
    )
}

fn main() -> Result<(), Box<Error>> {
    let input = read_input()?;

    // running each chunk in parallel means that the results will be flat_mapped
    // in an unspecified order. More intelligently connecting chunks could be
    // decent optimisation.
    chunk(input.clone())
        .par_iter()
        .flat_map(|chunk| solve_by_nearest(chunk))
        .for_each(|id| println!("{}", id));

    Ok(())
}
