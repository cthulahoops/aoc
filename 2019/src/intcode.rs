use std::collections::VecDeque;

#[derive(Clone)]
pub struct Program {
    pub memory: Vec<i64>,
    pub counter: usize,
    pub relative_base: i64,
    pub input: VecDeque<i64>,
}

pub fn read_program(filename: &str) -> Program {
    let memory = std::fs::read_to_string(filename)
        .unwrap()
        .lines()
        .next()
        .unwrap()
        .split(",")
        .map(|x| x.parse::<i64>().unwrap())
        .collect();
    Program::new(memory)
}

fn parameter_mode(instruction: i64, offset: usize) -> i64 {
    (instruction / (100 * 10_i64.pow(offset as u32 - 1))) % 10
}

impl Program {
    pub fn new(memory: Vec<i64>) -> Self {
        Program {
            memory,
            counter: 0,
            relative_base: 0,
            input: VecDeque::new(),
        }
    }

    fn instruction(&self) -> i64 {
        self.memory[self.counter]
    }

    fn get_parameter(&self, offset: usize) -> i64 {
        let arg = self.memory[self.counter + offset];
        // println!("::: {} {} {} {}", program[counter], offset, param_mode, arg);
        match parameter_mode(self.instruction(), offset) {
            0 => self.read(arg as usize),
            1 => arg,
            2 => self.read((arg + self.relative_base) as usize),
            other => panic!("Bad parameter mode: {}", other),
        }
    }

    fn write_parameter(&mut self, offset: usize, value: i64) {
        let arg = self.memory[self.counter + offset];
        match parameter_mode(self.instruction(), offset) {
            0 => self.write(arg as usize, value),
            1 => panic!("Can't write in param mode 1"),
            2 => self.write((arg + self.relative_base) as usize, value),
            other => panic!("Bad parameter mode: {}", other),
        }
    }

    fn read(&self, address: usize) -> i64 {
        if address >= self.memory.len() {
            return 0;
        }
        self.memory[address]
    }

    fn write(&mut self, address: usize, value: i64) {
        if address >= self.memory.len() {
            self.memory.resize(address + 1, 0)
        }
        self.memory[address] = value
    }

    fn binary_op<F>(&mut self, op: F)
    where
        F: FnOnce(i64, i64) -> i64,
    {
        let a = self.get_parameter(1);
        let b = self.get_parameter(2);
        self.write_parameter(3, op(a, b));
        self.counter += 4
    }

    fn jump_if<F>(&mut self, cond: F)
    where
        F: FnOnce(i64) -> bool,
    {
        let a = self.get_parameter(1);
        if cond(a) {
            let b = self.get_parameter(2) as usize;
            self.counter = b;
        } else {
            self.counter += 3;
        }
    }

    pub fn step(&mut self) -> Option<Option<i64>> {
        match self.memory[self.counter] % 100 {
            1 => self.binary_op(|a, b| a + b),
            2 => self.binary_op(|a, b| a * b),
            3 => {
                let value = self.input.pop_front().unwrap();
                self.write_parameter(1, value);
                self.counter += 2;
            }
            4 => {
                let a = self.get_parameter(1);
                self.counter += 2;
                return Some(Some(a));
            }
            5 => self.jump_if(|x| x != 0),
            6 => self.jump_if(|x| x == 0),
            7 => self.binary_op(|a, b| (a < b) as i64),
            8 => self.binary_op(|a, b| (a == b) as i64),
            9 => {
                self.relative_base += self.get_parameter(1);
                self.counter += 2;
            }
            99 => return None,
            other => panic!("Unexpected instruction: {} at {}", other, self.counter),
        }
        Some(None)
    }

    fn disassemble_arg(&self, counter: usize, offset: usize) -> String {
        let value : i64 = self.memory[counter + offset];
        match parameter_mode(self.memory[counter], offset) {
            0 => format!("a<{}>", value),
            1 => format!("{}", value),
            2 => format!("r<{}>", value),
            _ => format!("invalid<{}>", value),
        }
    }

    fn disassemble_args(&self, counter: usize, count: usize) -> Vec<String> {
        (1..count+1).map(|x| self.disassemble_arg(counter, x)).collect()
    }

    pub fn disassemble(&self) {
        let mut counter = 0;

        loop {
            if counter >= self.memory.len() {
                break;
            }
            let instruction_code = self.memory[counter];
            let (ins, args) = match instruction_code % 100 {
                1 => ("ADD".to_string(), 3),
                2 => ("MUL".to_string(), 3),
                3 => ("READ".to_string(), 1),
                4 => ("WRITE".to_string(), 1),
                5 => ("JUMP_IF_NON_ZERO".to_string(), 2),
                6 => ("JUMP_IF_ZERO".to_string(), 2),
                7 => ("LESS_THAN".to_string(), 3),
                8 => ("EQUAL".to_string(), 3),
                9 => ("SET_RELATIVE_BASE".to_string(), 1),
                99 => ("EXIT".to_string(), 0),
                other => (format!("{}", other), 0)
            };

            let dis_args = self.disassemble_args(counter, args);

            print!("{}: {} ", counter, ins);
            counter += args + 1;
            for x in dis_args {
                print!("{} ", x);
            }
            println!("");
        }
    }

    pub fn run<I>(&mut self, input: I)
    where
        I: Iterator<Item = i64>,
    {
        for item in input {
            self.send(item);
        }

        loop {
            match self.step() {
                Some(Some(output)) => {
                    println!("{}", output);
                }
                Some(None) => {}
                None => break,
            }
        }
    }

    pub fn send(&mut self, input: i64) {
        self.input.push_back(input);
    }
}

impl Iterator for Program {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.step() {
                Some(Some(output)) => {
                    return Some(output);
                }
                Some(None) => {}
                None => return None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Program;

    fn run_program(memory: Vec<i64>) -> Program {
        let mut program = Program::new(memory);
        program.run(vec![0].into_iter());
        program
    }

    #[test]
    fn negative_values() {
        let program = run_program(vec![1101, 100, -1, 4, 0]);
        assert_eq!(program.memory[4], 99);
    }

    #[test]
    fn modes() {
        let program = run_program(vec![1002, 4, 3, 4, 33]);
        assert_eq!(program.memory[4], 99);
    }

    #[test]
    fn output() {
        let program = Program::new(vec![104, 1, 104, 2, 104, 3, 99]);
        for x in program {
            println!("::: {}", x);
        }
    }
}
