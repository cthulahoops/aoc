use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;

fn mix(secret: i64, value: i64) -> i64 {
    secret ^ value
}

fn prune(secret: i64) -> i64 {
    secret % 16777216
}

fn mix_prune(secret: i64, stepped: i64) -> i64 {
    prune(mix(secret, stepped))
}

fn evolve_secret(mut secret: i64) -> i64 {
    secret = mix_prune(secret, secret * 64);
    secret = mix_prune(secret, secret / 32);
    mix_prune(secret, secret * 2048)
}

struct SecretSequence {
    current: i64,
}

impl SecretSequence {
    fn new(initial: i64) -> Self {
        SecretSequence {
            current: initial,
        }
    }
}

impl Iterator for SecretSequence {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.current;
        self.current = evolve_secret(self.current);
        Some(result)
    }
}

fn find_nth_number(start: i64, n: usize) -> i64 {
    SecretSequence::new(start)
        .nth(n)
        .expect("Failed to generate sequence")
}

fn read_input(file_path: &str) -> Vec<i64> {
    let file = File::open(file_path).expect("Failed to open file");
    let reader = io::BufReader::new(file);

    reader
        .lines()
        .map(|line| {
            line.expect("Failed to read line")
                .parse::<i64>()
                .expect("Failed to parse number")
        })
        .collect()
}

type MonkeyMemory = [i64; 4];
type Options = HashMap<MonkeyMemory, i64>;

const STEPS: usize = 2000;


fn sequence_options(start: i64) -> Options {
   // println!("Starting with: {}", start);
    let mut sequence = SecretSequence::new(start).take(STEPS + 1);

    let mut last = [
        // start % 10,  // Q: is the inital number counted?
        sequence.next().unwrap() % 10,
        sequence.next().unwrap() % 10,
        sequence.next().unwrap() % 10,
        sequence.next().unwrap() % 10,
    ];

//    println!("Initial last: {:?}", last);

    let mut options = HashMap::new();

    for number in sequence {
        let current = number % 10;

        let diffs = [
            last[1] - last[0],
            last[2] - last[1],
            last[3] - last[2],
            current - last[3],
        ];

        if !options.contains_key(&diffs) {
            options.insert(diffs, current);
        }

        last = [last[1], last[2], last[3], current];

    }

    options
}

fn best(options: std::collections::HashMap<[i64; 4], i64>) -> (i64, [i64; 4]) {
    let mut best_value = 0;
    let mut best_key = [0, 0, 0, 0];

    for (key, value) in options.iter() {
        if *value > best_value {
            best_key = *key;
            best_value = *value;
        }
    }

    (best_value, best_key)
}

pub fn print_great(options: std::collections::HashMap<[i64; 4], i64>) {
    for (key, value) in options.iter() {
        if *value > 20 {
            println!("{:?} -> {}", key, value);
        }
    }
}


fn add_options(options: Options, other: Options) -> Options {
    let mut result = options;

    for (key, value) in other.iter() {
        let old_value = result.get(key).unwrap_or(&0);
        result.insert(*key, old_value + value);
    }

    result
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];

    let input = read_input(file_path);

    let mut total = 0;

    for initial in &input {
        let result = find_nth_number(*initial, STEPS);
        total += result;
    }

    println!("Part 1: {}", total);

    let mut options = HashMap::new();

    for initial in input {
        options = add_options(options, sequence_options(initial));
    }

    // print_great(options.clone());

    let part2 = best(options);

    println!("Part 2: {:?}", part2);

    Ok(())
}
