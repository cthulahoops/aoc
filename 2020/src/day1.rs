use std::collections::HashSet;
// use std::collections::HashMap;
use std::fs;

fn main() {
    let contents = fs::read_to_string("input/day1").unwrap();
    let numbers = contents.lines().map(|x| x.parse().unwrap()).collect();

    println!("Part 1:");
    part1(&numbers);
    println!("Part 2:");
    part2(&numbers);
}

fn part1(numbers: &Vec<i64>) {
    let mut previous = HashSet::new();

    for number in numbers {
        if previous.contains(&(2020 - number)) {
            let a = number;
            let b = 2020 - number;
            println!("{} {} {}", a, b, a * b);
        }
        previous.insert(number);
    }
}

fn part2(numbers: &Vec<i64>) {
    let mut previous = HashSet::new();
    for a in numbers {
        for b in numbers {
            if a >= b {
                continue;
            }
            let c = 2020 - b - a;
            if previous.contains(&c) {
                println!("{} {} {} {}", a, b, c, a * b * c);
            }
        }
        previous.insert(a);
    }
}
