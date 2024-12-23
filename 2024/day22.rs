use std::env;
use std::fs::File;
use std::io::{self, BufRead};

fn mix(secret: u64, value: u64) -> u64 {
    secret ^ value
}

fn prune(secret: u64) -> u64 {
    secret % 16777216
}

fn evolve_secret(mut secret: u64) -> u64 {
    let result = secret * 64;
    secret = mix(secret, result);
    secret = prune(secret);

    let result = secret / 32;
    secret = mix(secret, result);
    secret = prune(secret);

    let result = secret * 2048;
    secret = mix(secret, result);
    secret = prune(secret);
    secret
}

struct SecretSequence {
    current: u64,
}

impl SecretSequence {
    fn new(initial: u64) -> Self {
        SecretSequence {
            current: initial,
        }
    }
}

impl Iterator for SecretSequence {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.current;
        self.current = evolve_secret(self.current);
        Some(result)
    }
}

fn find_nth_number(start: u64, n: usize) -> u64 {
    SecretSequence::new(start)
        .nth(n)
        .expect("Failed to generate sequence")
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    let file = File::open(file_path)?;
    let reader = io::BufReader::new(file);

    let mut total = 0;

    for line in reader.lines() {
        let initial = line?.parse::<u64>().unwrap_or_else(|_| {
            eprintln!("Error: Invalid number in input file");
            std::process::exit(1);
        });

        let result = find_nth_number(initial, 2000);
        total += result;
        println!("Starting from {}: 2000th number is {}", initial, result);
    }

    println!("Total: {}", total);

    Ok(())
}
