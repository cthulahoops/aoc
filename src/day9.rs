use std::collections::HashSet;
use std::collections::VecDeque;

fn is_valid(history: &VecDeque<i64>, candidate: i64) -> bool {
    let mut previous = HashSet::new();
    for item in history {
        if previous.contains(&(candidate - item)) {
            return true;
        }
        previous.insert(item);
    }
    false
}

fn invalid_number(numbers: &Vec<i64>) -> i64 {
    let ref mut numbers = numbers.iter().cloned();

    let mut last_25: VecDeque<i64> = numbers.take(25).collect();

    for number in numbers {
        if !is_valid(&last_25, number) {
            return number;
        }

        last_25.pop_front();
        last_25.push_back(number);
    }
    panic!("Nothing was invalid!");
}

fn main() {
    let contents = std::fs::read_to_string("input/day9").unwrap();
    let numbers: Vec<i64> = contents.lines().map(|x| x.parse().unwrap()).collect();

    let invalid_number = invalid_number(&numbers);
    println!("{}", invalid_number);

    for start in 0..numbers.len() {
        // println!("****\n{} ->\n{}", start, numbers[start]);
        let mut total = numbers[start];
        for end in start + 1..numbers.len() {
            total = total + numbers[end];
            //    println!("+ {} = {}", numbers[end], total);
            if total == invalid_number {
                println!(
                    "{} {} {} {} {}",
                    start,
                    end,
                    &numbers[start],
                    &numbers[end],
                    &numbers[start] + &numbers[end]
                );
                let range = &numbers[start..end + 1];
                let max = range.iter().cloned().fold(0, i64::max);
                let min = range.iter().cloned().fold(2 << 60, i64::min);
                println!("{:?}", min + max);
            }
            if total > invalid_number {
                break;
            }
        }
    }
}
