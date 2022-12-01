use lazy_static::lazy_static;
use regex::{Captures, Regex};

lazy_static! {
    static ref PAREN: Regex = Regex::new(r"\((([0-9]+ [+*] )+[0-9]+)\)").unwrap();
    static ref LEFT_MOST: Regex = Regex::new(r"^(\d+) ([+*]) (\d+)").unwrap();
    static ref ADDITION: Regex = Regex::new(r"(\d+) (\+) (\d+)").unwrap();
}

fn eval_single(captures: &Captures) -> String {
    let a = captures.get(1).unwrap().as_str().parse::<i64>().unwrap();
    let op = captures.get(2).unwrap().as_str();
    let b = captures.get(3).unwrap().as_str().parse::<i64>().unwrap();

    match op {
        "*" => (a * b).to_string(),
        "+" => (a + b).to_string(),
        _ => panic!("Not implemented"),
    }
    .to_string()
}

fn eval_left_most(s: &str) -> String {
    LEFT_MOST.replace(s, eval_single).to_string()
}

fn eval_simple_arith(s: &str) -> String {
    run_until_stable(eval_left_most, s)
}

fn eval_addition(s: &str) -> String {
    ADDITION.replace_all(s, eval_single).to_string()
}

fn eval_prio_additions(s: &str) -> String {
    let added = run_until_stable(eval_addition, s);
    eval_simple_arith(&added)
}

fn run_until_stable<F>(f: F, s: &str) -> String
where
    F: Fn(&str) -> String,
{
    let mut input = s.to_string();
    loop {
        let new_input = f(&input);
        if new_input == input {
            break;
        }
        input = new_input;
    }
    return input;
}

fn eval_expression(evaluator: fn(&str) -> String, s: &str) -> i64 {
    let input = run_until_stable(
        |input| {
            PAREN
                .replace_all(&input, |c: &Captures| evaluator(c.get(1).unwrap().as_str()))
                .to_string()
        },
        s,
    );
    evaluator(&input).parse::<i64>().unwrap()
}

fn eval1(s: &str) -> i64 {
    eval_expression(eval_simple_arith, s)
}

fn eval2(s: &str) -> i64 {
    eval_expression(eval_prio_additions, s)
}

fn main() -> anyhow::Result<()> {
    let contents = std::fs::read_to_string("input/day18").unwrap();

    let part1: i64 = contents.lines().map(eval1).sum();
    println!("Part 1 = {}", part1);

    println!("Example: {}", eval2("1 + 2 * 3 + 4 * 5 + 6"));

    let part2: i64 = contents.lines().map(eval2).sum();
    println!("Part 2 = {}", part2);

    Ok(())
}
