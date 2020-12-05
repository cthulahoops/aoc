use std::collections::HashSet;

fn main() {
    let content = std::fs::read_to_string("input/day5").unwrap();
    let mut seats = HashSet::new();
    let mut max = 0;
    let mut min = 2 << 11;
    for line in content.lines() {
        let mut number: i32 = 0;
        for x in line.chars() {
            number *= 2;
            if x == 'B' || x == 'R' {
                number += 1;
            }
        }
        if number > max {
            max = number;
        }
        if number < min {
            min = number;
        }
        seats.insert(number);
    }
    println!("{}", max);

    for seat in min..max {
        if !seats.contains(&seat) {
            println!("{}", seat);
        }
    }
}
