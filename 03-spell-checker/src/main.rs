extern crate regex;

use std::fs::File;
use std::collections::HashSet;
use std::io::{BufReader, BufRead};
use std::time::Instant;

use regex::Regex;

fn main() {
    let mut args = std::env::args();

    if args.len() != 3 {
        panic!("Two args pls: {} dictionary.txt file.txt", args.next().unwrap());
    }

    args.next(); // skip over executable name

    let dictionary = File::open(args.next().unwrap()).unwrap();
    let file = File::open(args.next().unwrap()).unwrap();

    let not_word = Regex::new(r"[^A-Za-z]").unwrap();

    let dictionary: HashSet<String> = BufReader::new(dictionary)
        .lines()
        .filter_map(Result::ok)
        .collect();

    let start = Instant::now();

    let errors = BufReader::new(file)
        .lines()
        .filter_map(Result::ok)
        .flat_map(|line| line.split_whitespace()
                             .map(|w| not_word.replace_all(&w, "").to_lowercase())
                             .collect::<Vec<_>>())
        .filter(|word| !dictionary.contains(word))
        .count();

    let duration = Instant::now() - start;

    println!("found {} errors in {:?}", errors, duration);
}
