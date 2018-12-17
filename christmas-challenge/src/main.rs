use std::hash::{Hash, Hasher}; use std::ops::Deref;
use std::cmp::Ordering;
use std::error::Error;
use std::io::{self, Read};
use std::str::FromStr;
use std::f64::{INFINITY, NEG_INFINITY};

use plotlib::scatter::{self, Scatter};
use plotlib::view::View;
use plotlib::page::Page;
use plotlib::style::{Point, Marker};

type Id = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Record {
    id: Id,
    lat: NotNan,
    lon: NotNan,
}

// Just hash the id!
impl Hash for Record {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}


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
        NotNan::from_f64(s.parse()?).ok_or(Box::<Error>::from("was NaN"))
    }
}

impl NotNan {
    fn from_f64(as_f64: f64) -> Option<Self> {
        if as_f64.is_nan() { None } else { Some(NotNan { as_f64 }) }
    }

    fn infinity() -> Self { NotNan::from_f64(INFINITY).unwrap() }

    fn neg_infinity() -> Self { NotNan::from_f64(NEG_INFINITY).unwrap() }
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

fn flat_distance((lat1, lon1): (f64, f64), (lat2, lon2): (f64, f64)) -> NotNan {
    NotNan::from_f64((lat1 - lat2).abs().hypot((lon1 - lon2).abs())).unwrap()
}

fn hav_distance((lat1, lon1): (f64, f64), (lat2, lon2): (f64, f64)) -> NotNan {
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

//fn solve_by_nearest(records: &[Record]) -> Vec<Id> {
//    let mut result = vec![records[1].clone()];
//    let mut records: HashSet<_> = records[1..].iter().collect();
//
//    loop {
//        let nearest = records.iter().min_by(|x, y| {
//            let record = result.last().unwrap();
//            let x_dist = hav_distance((*record.lat, *record.lon), (*x.lat, *x.lon));
//            let y_dist = hav_distance((*record.lat, *record.lon), (*y.lat, *y.lon));
//
//            x_dist.cmp(&y_dist)
//        });
//
//        match nearest {
//            None => return result.iter().map(|x| x.id).collect(),
//            Some(&nearest) => {
//                result.push(nearest.clone());
//                records.remove(nearest);
//            },
//        }
//
//    }
//}

const GRID_SIZE: usize = 10;

fn chunk(mut coords: Vec<Record>) -> Vec<Vec<Record>> {
    let min_max = ((NotNan::infinity(), NotNan::infinity()), (NotNan::neg_infinity(), NotNan::neg_infinity()));

    let ((min_lat, min_lon), (max_lat, max_lon)) = coords.iter().fold(min_max, |(min, max), coord| {
        let min_lat = std::cmp::min(coord.lat, min.0);
        let min_lon = std::cmp::min(coord.lon, min.1);
        let max_lat = std::cmp::max(coord.lat, max.0);
        let max_lon = std::cmp::max(coord.lon, max.1);
        ((min_lat, min_lon), (max_lat, max_lon))
    });

    let max_lat_norm = *max_lat - *min_lat;
    let max_lon_norm = *max_lon - *min_lon;

    coords.into_iter().fold(vec![Vec::new(); GRID_SIZE * GRID_SIZE], |mut grid, record| {
        let lat_norm = *record.lat - *min_lat;
        let lon_norm = *record.lon - *min_lon;
        let lat_bucket = ((GRID_SIZE as f64) * (lat_norm / max_lat_norm)).floor() as usize;
        let lon_bucket = ((GRID_SIZE as f64) * (lon_norm / max_lon_norm)).floor() as usize;
        grid[lat_bucket + lon_bucket].push(record);
        grid
    })
}

fn main() -> Result<(), Box<Error>> {
    let input = read_input()?;
    
    let colours = ["red", "blue", "green"];
    let mut colours_iter = colours.iter().cycle();

    let view = View::new();

    let data: Vec<_> = chunk(input).into_iter().map(|chunk| {
        let coords: Vec<_> = chunk.iter().map(|record| (*record.lat, *record.lon)).collect();
        Scatter::from_vec(&coords[..])
            .style(scatter::Style::new().colour(colours_iter.next().unwrap().clone()))
    }).collect();

    let view = data.iter().fold(view, |view, data| {
        view.add(data)
    });

    Page::single(&view).save("out2.svg");

    Ok(())
}
