use std::collections::VecDeque;
use std::collections::HashMap;

struct Cycle {
  edges: HashMap<usize, usize>
}

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

#[derive(Debug)]
struct ZipThing {
  left: VecDeque<usize>,
  current: usize,
  right: VecDeque<usize>
}

impl ZipThing {
  fn new(input: &Vec<usize>) -> Self {
    let mut iter = input.iter();
    let first = iter.next().unwrap();
    ZipThing { left: VecDeque::new(), current: *first, right: iter.cloned().collect() }
  }

  fn pop_next(&mut self) -> usize {
    match self.right.pop_front() {
      Some(value) => value,
      None => self.left.pop_front().unwrap()
    }
  }

  fn insert(&mut self, value: usize) {
    self.right.push_front(value)
  }

  fn insert_many(&mut self, values: &Vec<usize>) {
    for x in values.iter().rev() {
      self.insert(*x)
    }
  }

  fn take3(&mut self) -> Vec<usize> {
    vec![
      self.pop_next(),
      self.pop_next(),
      self.pop_next()
    ]
  }

  fn advance(&mut self) {
    self.left.push_back(self.current);
    self.current = self.pop_next();
  }

  fn find(&mut self, value: usize) {
    if value == self.current {
      return
    }
    match self.left.iter().position(|x| *x == value) {
      Some(idx) => {
        let mut mv_right = self.left.split_off(idx);
        self.right.push_front(self.current);
        self.current = mv_right.pop_front().unwrap();
        for x in mv_right.iter().rev() {
          self.right.push_front(*x)
        }
      }
      None => {
        let idx = self.right.iter().position(|x| *x == value).unwrap();
        let mut new_right = self.right.split_off(idx);
        let ref mv_left = self.right;

        self.left.push_back(self.current);
        self.current = new_right.pop_front().unwrap();
        for x in mv_left {
          self.left.push_back(*x)
        }
        self.right = new_right;
      }
    }
  }
    
}


fn find_local(input: &Vec<usize>, value: usize, hint: usize) -> usize {
  if input[hint] == value {
    return hint
  }

  let mut i = 1;
  loop {
    if hint + i < input.len() && input[hint + i] == value {
      return hint + i;
    }
    if hint > i && input[hint - i] == value {
      return hint - i;
    }
    
    i += 1
  }
}

fn do_step(zt: &mut ZipThing) {
  let current = zt.current;
    let picked_up = zt.take3();

    let mut destination_value = if current > 1 { current - 1 } else{ 1_000_000 };
    while find(&picked_up, destination_value).is_some() {
        if destination_value > 1 {
            destination_value = destination_value - 1;
        } else {
            destination_value = 1_000_000; // Max VALUE!
        }
    }

    zt.find(destination_value);

    zt.insert_many(&picked_up);
    // println!("{:?}", zt);
    zt.find(current);
    zt.advance();
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
    // let mut input = vec![3,8,9,1,2,5,4,6,7];
    let mut input = vec![3,2,6,5,1,9,4,7,8];
    for x in 10..1_000_000 + 1 {
          input.push(x);
    }
    let mut zt = ZipThing::new(&input);
    
    // for x in 10..1_000_000 + 1 {
    //      input.push(x);
    // }
    //
    println!("ZT: {:?}", zt);
    for i in 0..10_000_000 {
      if i % 10000 == 0{
        println!("\nRound: {}", i + 1);
      }
      do_step(&mut zt);
    }

    // println!("Done shuffle!");
    //
    print_cups(&input, 1);

   // // print_cups(&input, current_cup_idx);

    // for i in 0..10_000_000 {
    //     if (i + 1) % 10_000 == 0 {
    //         println!("\nRound: {}", i + 1);
    //     }
    // //    print_cups(&input, current_cup_idx);
    //     current_cup_idx = do_step(&mut input, current_cup_idx);
    //  //   print_cups(&input, current_cup_idx);
    // }

    // // println!("Done shuffle!");

    zt.find(1);
    println!("{:?}", zt);
    // let one_idx = find(&input, 1).unwrap();
    // println!("{} * {}", input[(one_idx + 1) % input.len()], input[(one_idx + 2) % input.len()]);
    // // for i in one_idx + 1..input.len() {
    // //     print!("{}", input[i])
    // // }
    // // for i in 0..one_idx {
    // //     print!("{}", input[i])
    // // }
    // // println!("");
}
