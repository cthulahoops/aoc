use aoclib::intcode::Program;

fn run_amplification_sequence(program: &Program, sequence: &Vec<i64>) -> i64 {
    let mut output = 0;
    for phase_setting in sequence {
        let mut program = program.clone();
        program.send(*phase_setting);
        program.send(output);
        output = program.next().unwrap();
    }
    output
}

fn run_feedback_loop(program: &Program, sequence: &Vec<i64>) -> i64 {
    let mut programs: Vec<Program> = sequence
        .iter()
        .map(|phase_setting| {
            let mut p = program.clone();
            p.send(*phase_setting);
            p
        })
        .collect();

    let mut output = 0;
    loop {
        for program in &mut programs {
            program.send(output);
            match program.next() {
                Some(out) => output = out,
                None => return output,
            }
        }
    }
}

fn find_best<F>(f: F, settings: Vec<i64>) -> (i64, Vec<i64>)
where
    F: Fn(&Vec<i64>) -> i64,
{
    let mut best = 0;
    let mut best_sequence = None;
    for sequence in permutations(settings.as_slice()) {
        let output = f(&sequence);
        if output > best {
            best = output;
            best_sequence = Some(sequence);
        }
    }
    (best, best_sequence.unwrap())
}
fn main() {
    let program: Program = aoclib::intcode::read_program("input/day7");

    let (best, best_sequence) = find_best(
        |sequence| run_amplification_sequence(&program, sequence),
        vec![0, 1, 2, 3, 4],
    );

    println!("Part 1 = {}", best);
    println!("Best sequence = {:?}", best_sequence);

    let (best, best_sequence) = find_best(
        |sequence| run_feedback_loop(&program, sequence),
        vec![5, 6, 7, 8, 9],
    );

    println!("Part 2 = {}", best);
    println!("Best sequence = {:?}", best_sequence);
}

fn permutations<T>(input: &[T]) -> Vec<Vec<T>>
where
    T: Clone,
{
    if input.len() == 1 {
        return vec![vec![input[0].clone()]];
    }
    let mut result = vec![];
    for perm in permutations(&input[1..]) {
        for i in 0..input.len() {
            let mut item = vec![];
            item.append(&mut perm[..i].to_vec());
            item.push(input[0].clone());
            item.append(&mut perm[i..].to_vec());
            result.push(item);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::permutations;
    use super::Program;

    #[test]
    fn test_example_1() {
        let example = vec![
            3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
        ];
        let program: Program = Program::new(example);
        assert_eq!(
            super::run_amplification_sequence(&program, &vec![4, 3, 2, 1, 0]),
            43210
        )
    }

    #[test]
    fn test_permutations() {
        assert_eq!(permutations(vec![1].as_slice()), vec![vec![1]]);
        assert_eq!(
            permutations(vec![1, 2].as_slice()),
            vec![vec![1, 2], vec![2, 1]]
        );
    }
}
