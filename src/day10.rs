use std::collections::HashSet;
use std::collections::HashMap;

fn parse_sorted_numbers(contents: &str) -> Vec<i64> {
    let mut numbers : Vec<i64> = contents.lines().map(|x| x.parse().unwrap()).collect();
    numbers.sort();
    numbers
}

fn arrangements(cache: &mut HashMap<i64, i64>, adaptors: &HashSet<i64>, n: i64) -> i64 {
    if let Some(value) = cache.get(&n) {
        return value.clone();
    }

    let mut count = 0;
    for x in 1..4 {
        if adaptors.contains(&(n - x)) {
            count += arrangements(cache, adaptors, n - x);
        }
    }

    if n <= 3 {
        count += 1;
    }

    cache.insert(n, count);

    count
}



fn main() {
    let contents = std::fs::read_to_string("input/day10").unwrap();
    let numbers = parse_sorted_numbers(&contents);

    let mut last = 0;
    let mut ones = 0;
    let mut threes = 0;

    for number in &numbers {
        let gap = number - last;
        if gap == 3 {
            threes += 1;
        } else if gap == 1 {
            ones += 1;
        }
        last = number.clone();
    }

    threes += 1; // Yuck!

    println!("Ones = {}\nThrees = {}\nPart 1 = {}", ones, threes, ones * threes);

    let target = last + 3;
    let adaptors : HashSet<i64> = numbers.iter().cloned().collect();
   
    let mut cache = HashMap::new();
    println!("Part 2 = {}\n", arrangements(&mut cache, &adaptors, target));
}
