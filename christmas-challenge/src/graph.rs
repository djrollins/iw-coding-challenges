use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::f64::{INFINITY, MIN, NEG_INFINITY};
use std::hash::{Hash, Hasher};
use std::io::{self, Read};
use std::ops::Deref;
use std::str::FromStr;

use plotlib::{
    line::{self, Style},
    page::Page,
    style::Line,
    view::ContinuousView,
};

type Id = u32;

#[derive(Debug, Clone, PartialEq)]
struct Record {
    id: Id,
    lat: f64,
    lon: f64,
}

// Just hash the id!
impl Hash for Record {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

fn read_result() -> Result<Vec<Id>, Box<Error>> {
    let mut input = String::new();
    let mut file = std::fs::File::open("best_window.txt")?;
    file.read_to_string(&mut input)?;
    let input: Result<Vec<_>, _> = input
        .lines()
        .map(|line| line.parse())
        .collect();
    Ok(input?)
}

fn read_input() -> Result<Vec<Record>, Box<Error>> {
    let mut input = String::new();
    let mut records = Vec::new();
    let mut file = std::fs::File::open("cities500.csv")?;

    file.read_to_string(&mut input)?;

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


impl Eq for Record {}

fn main() -> Result<(), Box<Error>> {
    let input = read_input()?;
    let result = read_result()?;

    let as_map: HashMap<_, _> = input.iter().map(|record| {
        (record.id, (record.lat, record.lon))
    }).collect();

    let result: Vec<_> = result.iter().map(|r| as_map[r]).collect();

    let data = line::Line::new(&result)
        .style(line::Style::new().colour("black"));

    let view = ContinuousView::new().add(&data);
    Page::single(&view)
        .save("out2.svg")
        .expect("saving");

    Ok(())
}
