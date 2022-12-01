use aoclib::intcode::Program;

fn main() {
    let mut program: Program = aoclib::intcode::read_program("input/day9");

    program.clone().run(vec![1].into_iter());

    program.run(vec![2].into_iter());
}

#[cfg(test)]
mod tests {
    use super::Program;

    #[test]
    fn test_example_quine() {
        let example = vec![
            109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99,
        ];
        let result: Vec<i64> = Program::new(example.clone()).collect();
        assert_eq!(example, result);
    }

    #[test]
    fn test_large1() {
        let example = vec![1102, 34915192, 34915192, 7, 4, 7, 99, 0];
        let output = Program::new(example).next().unwrap();
        assert_eq!(output, 1219070632396864);
    }

    #[test]
    fn test_large2() {
        let example = vec![104, 1125899906842624, 99];
        let output = Program::new(example).next().unwrap();
        assert_eq!(output, 1125899906842624);
    }
}
