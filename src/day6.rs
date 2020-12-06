use std::collections::HashSet;

fn answers(line: &str) -> HashSet<char> {
    line.chars().collect()
}

fn main() {
    let content = std::fs::read_to_string("input/day6").unwrap();

    let mut total_part_1 = 0;
    let mut total_part_2 = 0;

    for group in content.split("\n\n") {
        let mut people = group.trim_end().split("\n");
        let first_answers = answers(people.next().unwrap());

        let group_answers_1 = people
            .clone()
            .fold(first_answers.clone(), |group_answers, person| {
                group_answers.union(&answers(person)).cloned().collect()
            });

        let group_answers_2 = people.fold(first_answers, |group_answers, person| {
            group_answers
                .intersection(&answers(person))
                .cloned()
                .collect()
        });

        total_part_1 += group_answers_1.len();
        total_part_2 += group_answers_2.len();
    }

    println!("Total = {}", total_part_1);
    println!("Total = {}", total_part_2);
}
