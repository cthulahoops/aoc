#![feature(map_into_keys_values)]

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
enum Command {
    SetMask(usize, usize),
    Assign(usize, i64),
}

lazy_static! {
    static ref SET_MASK: Regex = Regex::new(r"^mask = ([10X]{36})$").unwrap();
    static ref ASSIGN: Regex = Regex::new(r"^mem\[([0-9]+)\] = ([0-9]+)$").unwrap();
}

// fn parse_bits(s: &str) -> anyhow::Result<i64> {
//     let length = s.len();
//     let mut result = 0;
//     for (i, x) in s.chars().enumerate() {
//         match x {
//             '1' => result &= 1 << (length - i),
//             '0' => (),
//             _ => return Err(anyhow::anyhow!("Parse error"))
//         }
//     }
//     Ok(result)
// }

fn parse_line(s: &str) -> anyhow::Result<Command> {
    if let Some(captures) = SET_MASK.captures(s) {
        let mut bitmask = 0;
        let mut address = 0;
        for (i, bit) in captures.get(1).unwrap().as_str().chars().enumerate() {
            let x = 1 << (35 - i);
            match bit {
                '1' => address |= x,
                '0' => (),
                'X' => bitmask |= x,
                _ => return Err(anyhow::anyhow!("Invalid character in mask: {}", bit)),
            }
        }

        return Ok(Command::SetMask(bitmask, address));
    }

    if let Some(captures) = ASSIGN.captures(s) {
        let address = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
        let value = captures.get(2).unwrap().as_str().parse::<i64>().unwrap();
        return Ok(Command::Assign(address, value));
    }

    Err(anyhow::anyhow!("Can't parse: {}", s))
}

// fn part1(commands: &Vec<Command>) -> anyhow::Result<()> {
//     let mut mask : Vec<(usize, bool)> = vec![];
//     let mut memory : HashMap<usize, i64> = HashMap::new();

//     for command in commands {
//         match command {
//             Command::SetMask(new_mask) => mask = new_mask.clone(),
//             Command::Assign(address, mut value) => {
//                 for (i, bit) in &mask {
//                     if *bit {
//                         value |= 1 << i;
//                     } else {
//                         value &= !(1 << i);
//                     }
//                 }
//                 memory.insert(*address, value);
//             }
//         }
//     }
//     println!("{}", memory.into_values().sum::<i64>());
//     Ok(())
// }

fn format_mask(floating_mask: usize, address: usize) -> String {
    (0..36)
        .rev()
        .map(|i| {
            if floating_mask & (1 << i) != 0 {
                'X'
            } else if address & (1 << i) != 0 {
                '1'
            } else {
                '0'
            }
        })
        .collect()
}

fn count_bits(mut x: usize) -> usize {
    println!("Counting: {}", x);
    let mut count = 0;
    while x != 0 {
        if x & 1 != 0 {
            count += 1;
        }
        x = x >> 1
    }
    count
}

fn expand_addresses(mask: usize, address: usize) -> Vec<usize> {
    let mut result = vec![address];

    println!("Expand: {}, {}", mask, address);

    for i in 0..36 {
        let x = 1 << i;

        if mask & x != 0 {
            let mut temp_result = vec![];
            for address in result {
                temp_result.push(address | x);
                temp_result.push(address & !x);
            }
            result = temp_result
        }
    }
    result
}

fn part2(commands: &Vec<Command>) -> anyhow::Result<()> {
    let mut mask: (usize, usize) = (0, 0);
    let mut memory: HashMap<usize, i64> = HashMap::new();

    for command in commands {
        println!("Command: {:?}", command);
        match command {
            Command::SetMask(bitmask, address) => {
                //   mask = new_mask.clone();
                mask = (*bitmask, *address);
                println!(
                    "Set: {}, {}, {}",
                    bitmask,
                    address,
                    format_mask(*bitmask, *address)
                );
            }
            Command::Assign(address, value) => {
                let address = (address | mask.1) & !mask.0;

                println!("{} = {}", format_mask(mask.0, address | mask.1), value);

                for address in expand_addresses(mask.0, address | mask.1) {
                    println!("{} <- {}", address, value);
                    memory.insert(address, *value);
                }
            }
        }
    }

    println!("Done, summing.");

    println!("{}", memory.into_values().sum::<i64>());

    Ok(())
}

fn main() -> anyhow::Result<()> {
    let commands = aoclib::read_parsed_lines("input/day14", parse_line)?;

    // part1(&commands)?;
    part2(&commands)?;

    Ok(())
}
