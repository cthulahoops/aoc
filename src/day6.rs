use std::collections::HashSet;

type Answers = HashSet<char>;

fn answers(line: &str) -> Answers {
    line.chars().collect()
}

fn group_answers<F: Fn(&Answers, &Answers) -> Answers>(
    group: &Vec<Answers>,
    op: F,
) -> HashSet<char> {
    let (first_answers, rest_answers) = group.split_first().unwrap();

    rest_answers.iter()
        .fold(first_answers.clone(), |group_answers, answer| {
            op(&group_answers, answer)
        })
}

fn sum_counts<I: Iterator<Item = Answers>>(grouped_answers: I) -> usize {
    grouped_answers.map(|x| x.len()).sum()
}

fn main() {
    let content = std::fs::read_to_string("input/day6").unwrap();

    let groups = content.split("\n\n").map(|group| group.trim_end().split('\n').map(answers).collect());

    let group_answers_1 = groups
        .clone()
        .map(|group| {
            group_answers(&group, |group_answers, answers| {
                group_answers.union(answers).cloned().collect()
            })
        });

    let group_answers_2 = groups
        .map(|group| {
            group_answers(&group, |group_answers, answers| {
                group_answers.intersection(answers).cloned().collect()
            })
        });

    println!("Total = {}", sum_counts(group_answers_1));
    println!("Total = {}", sum_counts(group_answers_2));
}
