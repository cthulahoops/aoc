use aoclib::intcode;

fn main() {
    let mut program: intcode::Program = intcode::read_program("input/day5");
    program.run(vec![1].into_iter());

    let mut program: intcode::Program = intcode::read_program("input/day5");
    program.run(vec![5].into_iter());
}
