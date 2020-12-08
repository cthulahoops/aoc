use std::collections::HashSet;
use std::str::FromStr;

#[derive(Debug, Copy, Clone)]
enum Instruction {
    Nop,
    Acc,
    Jmp,
}

impl FromStr for Instruction {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "nop" => Ok(Instruction::Nop),
            "acc" => Ok(Instruction::Acc),
            "jmp" => Ok(Instruction::Jmp),
            _ => Err(anyhow::anyhow!("Invalid instruction: {:?}", s)),
        }
    }
}

#[derive(Clone)]
struct Program {
    instructions: Vec<(Instruction, i64)>,
}

impl FromStr for Program {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, anyhow::Error> {
        let program = s
            .lines()
            .map(|x| {
                let mut parts = x.split(" ");
                let instruction = parts
                    .next()
                    .ok_or(anyhow::anyhow!("Missing instruction"))?
                    .parse::<Instruction>()?;
                let value = parts
                    .next()
                    .ok_or(anyhow::anyhow!("Missing value"))?
                    .parse::<i64>()?;
                Ok((instruction, value))
            })
            .collect::<Result<Vec<(Instruction, i64)>, anyhow::Error>>()?;
        Ok(Program {
            instructions: program,
        })
    }
}

struct Vm {
    pointer: usize,
    acc: i64,
    program: Program,
}

impl Vm {
    fn new(program: Program) -> Self {
        Vm {
            pointer: 0,
            acc: 0,
            program,
        }
    }

    fn halted(&self) -> bool {
        self.pointer >= self.program.instructions.len()
    }

    fn step(&mut self) {
        let ref instruction = self.program.instructions[self.pointer as usize];

        match instruction {
            (Instruction::Nop, _) => {
                self.pointer += 1;
            }
            (Instruction::Acc, x) => {
                self.acc += x;
                self.pointer += 1;
            }
            (Instruction::Jmp, x) => {
                self.pointer = (self.pointer as i64 + x) as usize;
            }
        }
    }
}

fn try_halts(program: Program) -> Result<i64, i64> {
    let mut vm = Vm::new(program);
    let mut executed: HashSet<usize> = HashSet::new();

    loop {
        let pointer = vm.pointer;
        if executed.contains(&pointer) {
            return Err(vm.acc);
        }
        vm.step();
        if vm.halted() {
            return Ok(vm.acc);
        }
        executed.insert(pointer);
    }
}

fn change_instruction(program: &Program, at: usize) -> Option<Program> {
    if let Some(new_instruction) = match program.instructions[at].0 {
        Instruction::Nop => Some(Instruction::Jmp),
        Instruction::Jmp => Some(Instruction::Nop),
        Instruction::Acc => None,
    } {
        let mut program = program.clone();
        program.instructions[at].0 = new_instruction;
        return Some(program);
    }
    None
}

fn main() -> anyhow::Result<()> {
    let content = std::fs::read_to_string("input/day8")?;
    let program = content.parse::<Program>()?;

    let loop_acc = try_halts(program.clone()).expect_err("Program halted unexpectedly");
    println!("Part 1 = {}", loop_acc);

    for i in 0..program.instructions.len() {
        if let Some(program) = change_instruction(&program, i) {
            if let Ok(acc) = try_halts(program) {
                println!("Part 2 = {}", acc);
                break;
            }
        }
    }

    Ok(())
}
