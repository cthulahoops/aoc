use aoclib::intcode::Program;

fn run_program(program: &Program, noun: i64, verb: i64) -> i64 {
    let mut program = program.clone();

    program.memory[1] = noun;
    program.memory[2] = verb;

    program.run(vec![0].into_iter());
    return program.memory[0];
}

fn find_inputs(program: &Program, target: i64) -> Option<(i64, i64)> {
    for noun in 0..100 {
        for verb in 0..100 {
            let result = run_program(&program, noun, verb);
            if result == target {
                return Some((noun, verb));
            }
        }
    }
    None
}

fn main() {
    let program: Program = aoclib::intcode::read_program("input/day2");

    let part1 = run_program(&program, 12, 2);
    println!("Program 0: {}", part1);

    let (noun, verb) = find_inputs(&program, 19690720).unwrap();

    println!(
        "Noun = {}, verb = {}, part 2 = {}",
        noun,
        verb,
        100 * noun + verb
    );
}
