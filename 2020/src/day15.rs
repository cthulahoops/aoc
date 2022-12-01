use std::collections::HashMap;

fn game(starting_numbers: Vec<usize>, count: usize) -> usize {
    let mut memory: HashMap<usize, usize> = HashMap::new();
    let mut last_number = 0;

    for (turn, number) in starting_numbers.iter().enumerate() {
        if turn > 0 {
            memory.insert(last_number, turn);
        }
        last_number = *number;
    }

    for turn in starting_numbers.len()..count {
        let current_number = match memory.get(&last_number) {
            None => 0,
            Some(last_seen) => turn - last_seen,
        };

        memory.insert(last_number, turn);
        last_number = current_number;
    }

    last_number
}

fn main() {
    println!("Part 1 (example) = {}", game(vec![0, 3, 6], 2020));
    println!("Part 1 = {}", game(vec![14, 1, 17, 0, 3, 20], 2020));
    println!("Part 2 = {}", game(vec![14, 1, 17, 0, 3, 20], 30000000));
}
