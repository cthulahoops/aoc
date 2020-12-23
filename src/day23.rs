fn find(input: &Vec<usize>, value: usize) -> Option<usize> {
    input.iter().position(|x| *x == value)
}

// fn find_destination(input: &Vec<usize>, mut n: usize) -> usize {
//     loop {
//         match find(input, n) {
//             Some(i) => return i,
//             None => if n > 1 { n -= 1 } else { n = 9 }
//         }
//     }
// }
//


fn do_step(input: &mut Vec<usize>, n: usize) -> usize {
    let count = input.len();
    let current_value = input[n];

    let mut picked_up = vec![];
    for i in 0..3 {
        picked_up.push(input[(n + i + 1) % count])
    }

    // println!("Picked up: {:?}", picked_up);

    let mut destination_value = if current_value > 1 { current_value - 1 } else{ 1_000_000 };
    while find(&picked_up, destination_value).is_some() {
        if destination_value > 1 {
            destination_value = destination_value - 1;
        } else {
            destination_value = 1_000_000; // Max VALUE!
        }
    }
    // println!("Destination value: {}", destination_value);
    let mut destination = find(&input, destination_value).unwrap();

    if destination > n {
        for i in n+1..destination-2 {
            input[i] = input[i + 3];
        }

        destination -= 3;

    } else {
       // println!("Shuffle forward!");
        for i in (destination..n).rev() {
            input[(i+4) % count] = input[(i+1) % count];
        }
    }
    for i in 0..3 {
        input[(destination + i + 1) % count] = picked_up[i];
    }
    // println!("Picked up: {:?}", picked_up);

    // let destination = find_destination(input, current_value - 1);

    // for x in picked_up.iter().rev() {
    //     input.insert(destination + 1, *x);
    // }

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
  //  let mut input = vec![3,8,9,1,2,5,4,6,7];
    let mut input = vec![3,2,6,5,1,9,4,7,8];
    for x in 10..1_000_000 + 1 {
         input.push(x);
    }

    let mut current_cup_idx = 0;

   // print_cups(&input, current_cup_idx);

    for i in 0..10_000_000 {
        if (i + 1) % 10_000 == 0 {
            println!("\nRound: {}", i + 1);
        }
    //    print_cups(&input, current_cup_idx);
        current_cup_idx = do_step(&mut input, current_cup_idx);
     //   print_cups(&input, current_cup_idx);
    }

    // println!("Done shuffle!");
    //
    print_cups(&input, 1);

    let one_idx = find(&input, 1).unwrap();
    println!("{} * {}", input[(one_idx + 1) % input.len()], input[(one_idx + 2) % input.len()]);
    // for i in one_idx + 1..input.len() {
    //     print!("{}", input[i])
    // }
    // for i in 0..one_idx {
    //     print!("{}", input[i])
    // }
    // println!("");
}
