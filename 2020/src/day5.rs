use std::collections::HashSet;

fn parse_seat_number(code: &str) -> i32 {
    let mut number = 0;
    for x in code.chars() {
        number *= 2;
        if x == 'B' || x == 'R' {
            number += 1;
        }
    }
    number
}

fn main() {
    let content = std::fs::read_to_string("input/day5").unwrap();
    let seats: HashSet<i32> = content.lines().map(parse_seat_number).collect();

    let min = seats.iter().cloned().fold(2 << 11, i32::min);
    let max = seats.iter().cloned().fold(0, i32::max);

    println!("{}", max);

    for seat in min..max {
        if !seats.contains(&seat) {
            println!("{}", seat);
        }
    }
}
