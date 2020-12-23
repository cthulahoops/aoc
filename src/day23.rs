use std::collections::HashMap;

struct Cycle {
    edges: HashMap<usize, usize>,
    max_value: usize,
}

impl Cycle {
    fn new(input: &Vec<usize>) -> Self {
        let mut edges: HashMap<usize, usize> = HashMap::new();

        let mut iter = input.iter();
        let first = iter.next().unwrap();
        let mut last: usize = *first;
        let mut max_value: usize = *first;
        for x in iter {
            if *x > max_value {
                max_value = *x
            }
            edges.insert(last, *x);
            last = *x;
        }
        edges.insert(last, *first);

        Cycle { edges, max_value }
    }

    fn take_at(&mut self, value: usize) -> usize {
        let result = self.edges.remove(&value).unwrap();
        let following = self.edges.remove(&result).unwrap();
        self.edges.insert(value, following);
        result
    }

    fn take3(&mut self, value: usize) -> Vec<usize> {
        vec![
            self.take_at(value),
            self.take_at(value),
            self.take_at(value),
        ]
    }

    fn insert(&mut self, at: usize, value: usize) {
        let following = self.edges.remove(&at).unwrap();
        self.edges.insert(at, value);
        self.edges.insert(value, following);
    }

    fn insert_many(&mut self, at: usize, values: Vec<usize>) {
        for x in values.iter().rev() {
            self.insert(at, *x)
        }
    }

    fn display(&self) {
        let mut value = 1;
        loop {
            print!("{} ", value);
            value = self.edges[&value];
            if value == 1 {
                break;
            }
        }
        println!("");
    }

    fn contains(&self, value: usize) -> bool {
        self.edges.contains_key(&value)
    }

    fn next(&self, at: usize) -> usize {
        self.edges[&at]
    }
}

fn do_step(cycle: &mut Cycle, current_value: usize) -> usize {
    // println!("Current: {}", current_value);
    let picked_up = cycle.take3(current_value);

    // println!("Picked up: {:?}", picked_up);
    // cycle.display();

    let mut destination_value = if current_value > 1 {
        current_value - 1
    } else {
        cycle.max_value
    };
    while !cycle.contains(destination_value) {
        if destination_value > 1 {
            destination_value = destination_value - 1;
        } else {
            destination_value = cycle.max_value;
        }
    }

    // println!("Destination value: {}", destination_value);

    cycle.insert_many(destination_value, picked_up);
    cycle.next(current_value)
}

fn run_game(input: Vec<usize>, rounds: usize) -> Cycle {
    let mut cycle = Cycle::new(&input);

    //  cycle.display();

    let mut current_value = input[0];
    for _round in 1..rounds + 1 {
        // println!("\nRound: {}", round);
        current_value = do_step(&mut cycle, current_value);
        //      cycle.display();
    }
    cycle
}

fn main() {
    let input = vec![3, 8, 9, 1, 2, 5, 4, 6, 7];
    // let mut input = vec![3,2,6,5,1,9,4,7,8];
    // for x in 10..1_000_000 + 1 {
    //       input.push(x);
    // }
    let cycle = run_game(input, 100);
    cycle.display();

    let mut input = vec![3, 2, 6, 5, 1, 9, 4, 7, 8];
    for x in 10..1_000_000 + 1 {
        input.push(x);
    }
    let cycle = run_game(input, 10_000_000);
    let a = cycle.next(1);
    let b = cycle.next(a);
    println!("{} * {} = {}", a, b, a * b);
}
