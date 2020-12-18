use lazy_static::lazy_static;
use regex::{Captures, Regex};

lazy_static! {
    static ref PAREN: Regex = Regex::new(r"\((([0-9]+ [+*] )+[0-9]+)\)").unwrap();
}

fn replacer<F>(evaluator: F, captures: &Captures) -> String
where
    F: Fn(&str) -> String,
{
    evaluator(captures.get(1).unwrap().as_str())
}

fn eval_simple_arith(s: &str) -> String {
    let mut tokens = s.split(' ');

    let mut total: i64 = tokens.next().unwrap().parse::<i64>().unwrap();

    loop {
        let f = match tokens.next() {
            Some("*") => |a, b| a * b,
            Some("+") => |a, b| a + b,
            Some(_) => {
                panic!("Impossible!");
            }
            None => {
                break;
            }
        };

        let b: i64 = tokens.next().unwrap().parse::<i64>().unwrap();

        total = f(total, b);
    }

    total.to_string()
}

enum Op {
    Add,
    Mul,
}

fn eval_prio_additions(s: &str) -> String {
    let mut tokens = s.split(' ');

    let mut sums: Vec<i64> = vec![];
    let mut total: i64 = tokens.next().unwrap().parse::<i64>().unwrap();

    loop {
        let op = match tokens.next() {
            Some("+") => Op::Add,
            Some("*") => Op::Mul,
            Some(_) => {
                panic!("Impossible!");
            }
            None => {
                break;
            }
        };
        let b: i64 = tokens.next().unwrap().parse::<i64>().unwrap();

        match op {
            Op::Add => total += b,
            Op::Mul => {
                sums.push(total);
                total = b
            }
        }
    }
    sums.push(total);

    let result: i64 = sums.iter().product();

    result.to_string()
}

fn eval_expression(evaluator: fn(&str) -> String, s: &str) -> i64 {
    let mut input = s.to_string();
    while input.contains('(') {
        input = PAREN
            .replace_all(&input, |c: &Captures| replacer(evaluator, c))
            .to_string();
    }
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
