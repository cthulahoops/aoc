fn find(input: &Vec<usize>, value: usize) -> Option<usize> {
    input.iter().position(|x| *x == value)
}

fn find_destination(input: &Vec<usize>, mut n: usize) -> usize {
    loop {
        match find(input, n) {
            Some(i) => return i,
            None => if n > 1 { n -= 1 } else { n = 9 }
        }
    }
}

fn do_step(input: &mut Vec<usize>, n: usize) -> usize {
    let current_value = input[n];

    let mut picked_up = vec![];
    for _ in 0..3 {
        picked_up.push(input.remove(if n + 1 < input.len() { n + 1 } else { 0  }));
    }
    // println!("Picked up: {:?}", picked_up);

    let destination = find_destination(input, current_value - 1);

    for x in picked_up.iter().rev() {
        input.insert(destination + 1, *x);
    }

    (find(input, current_value).unwrap() + 1) % input.len()
}


fn print_cups(cups: &Vec<usize>, current: usize) {
    for (i, x) in cups.iter().enumerate() {
        if i == current {
            print!("({}) ", x);
        } else {
            print!("{} ", x);
        }
    }
    println!("");
}

fn main() {
//    let mut input = vec![3,8,9,1,2,5,4,6,7];
    let mut input = vec![3,2,6,5,1,9,4,7,8];
    // for x in 10..1_000_000 + 1 {
    //     input.push(x);
    // }

    let mut current_cup_idx = 0;

   print_cups(&input, current_cup_idx);

    for i in 0..10 {
        if i % 1000 == 0 {
            println!("i = {}", i);
        }
        current_cup_idx = do_step(&mut input, current_cup_idx);
        println!("");
        print_cups(&input, current_cup_idx);
    }

    let one_idx = find(&input, 1).unwrap();
    for i in one_idx + 1..input.len() {
        print!("{}", input[i])
    }
    for i in 0..one_idx {
        print!("{}", input[i])
    }
    println!("");
}
