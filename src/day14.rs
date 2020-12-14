#![feature(map_into_keys_values)]

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
struct Masked {
    mask: usize,
    value: usize,
}

impl Masked {
    #[allow(dead_code)]
    pub fn format(&self) -> String {
        (0..36)
            .rev()
            .map(|i| {
                if self.mask & (1 << i) != 0 {
                    'X'
                } else if self.value & (1 << i) != 0 {
                    '1'
                } else {
                    '0'
                }
            })
            .collect()
    }
}

#[derive(Debug)]
enum Command {
    SetMask(Masked),
    Assign(usize, usize),
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

        return Ok(Command::SetMask(Masked {
            mask: bitmask,
            value: address,
        }));
    }

    if let Some(captures) = ASSIGN.captures(s) {
        let address = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
        let value = captures.get(2).unwrap().as_str().parse::<usize>().unwrap();
        return Ok(Command::Assign(address, value));
    }

    Err(anyhow::anyhow!("Can't parse: {}", s))
}

struct Machine {
    mask: Masked,
    memory: HashMap<usize, usize>,
}

impl Machine {
    fn new() -> Self {
        Machine {
            mask: Masked { mask: !0, value: 0 },
            memory: HashMap::new(),
        }
    }

    fn store(&mut self, key: usize, value: usize) {
        self.memory.insert(key, value);
    }

    fn sum_values(&self) -> usize {
        self.memory.clone().into_values().sum()
    }
}

fn part1(commands: &Vec<Command>) -> anyhow::Result<usize> {
    let mut machine = Machine::new();

    for command in commands {
        match command {
            Command::SetMask(mask) => machine.mask = *mask,
            Command::Assign(address, value) => {
                let ones = !machine.mask.mask & machine.mask.value;
                let zeros = !machine.mask.mask & !machine.mask.value;

                let value = value & !zeros | ones;
                machine.store(*address, value);
            }
        }
    }
    Ok(machine.sum_values())
}

fn expand_addresses(mask: usize, address: usize) -> Vec<usize> {
    let mut result = vec![address];

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

fn part2(commands: &Vec<Command>) -> anyhow::Result<usize> {
    let mut machine = Machine::new();

    for command in commands {
        match command {
            Command::SetMask(mask) => {
                machine.mask = mask.clone();
            }
            Command::Assign(address, value) => {
                let address = (address | machine.mask.value) & !machine.mask.mask;

                for address in expand_addresses(machine.mask.mask, address | machine.mask.value) {
                    // println!("{} <- {}", address, value);
                    machine.store(address, *value);
                }
            }
        }
    }

    Ok(machine.sum_values())
}

fn main() -> anyhow::Result<()> {
    let commands = aoclib::read_parsed_lines("input/day14", parse_line)?;

    println!("Part 1 = {}", part1(&commands)?);
    println!("Part 2 = {}", part2(&commands)?);

    Ok(())
}
