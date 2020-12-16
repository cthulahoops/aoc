#![feature(iterator_fold_self)]

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
struct Rule {
    field_name: String,
    ranges: Vec<(usize, usize)>,
}

impl Rule {
    fn is_valid(&self, value: usize) -> bool {
        self.ranges
            .iter()
            .any(|(min, max)| value >= *min && value <= *max)
    }
}

lazy_static! {
    static ref RULE: Regex = Regex::new(r"^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$").unwrap();
}

fn parse_rule(s: &str) -> anyhow::Result<Rule> {
    let captures = RULE.captures(s).unwrap();

    let field_name = captures.get(1).unwrap().as_str();

    let a1 = captures.get(2).unwrap().as_str().parse::<usize>().unwrap();
    let a2 = captures.get(3).unwrap().as_str().parse::<usize>().unwrap();
    let b1 = captures.get(4).unwrap().as_str().parse::<usize>().unwrap();
    let b2 = captures.get(5).unwrap().as_str().parse::<usize>().unwrap();

    Ok(Rule {
        field_name: field_name.to_string(),
        ranges: vec![(a1, a2), (b1, b2)],
    })
}

type Ticket = Vec<usize>;

fn parse_ticket(s: &str) -> anyhow::Result<Ticket> {
    Ok(s.split(",").map(|x| x.parse::<usize>().unwrap()).collect())
}

fn parse_tickets(s: &str) -> anyhow::Result<Vec<Ticket>> {
    s.lines().skip(1).map(parse_ticket).collect()
}

fn ticket_scanning_error(ticket: &Ticket, rules: &Vec<Rule>) -> usize {
    let mut scanning_error = 0;
    for field in ticket {
        if !rules.iter().any(|rule| rule.is_valid(*field)) {
            scanning_error += field;
        }
    }
    return scanning_error;
}

fn possibilities(ticket: &Ticket, rules: &Vec<Rule>) -> Vec<HashSet<usize>> {
    ticket
        .iter()
        .map(|field| {
            rules
                .iter()
                .enumerate()
                .filter_map(|(i, rule)| if rule.is_valid(*field) { Some(i) } else { None })
                .collect()
        })
        .collect()
}

fn solve_for_order(possibilities: &Vec<HashSet<usize>>, allocated: &mut Vec<usize>) -> bool {
    let num_allocated = allocated.len();

    if num_allocated == possibilities.len() {
        return true;
    }

    let ref current = possibilities[num_allocated];

    for item in current {
        if allocated.contains(&item) {
            continue;
        }

        allocated.push(*item);

        if solve_for_order(possibilities, allocated) {
            return true;
        }
        allocated.pop();
    }
    return false;
}

fn main() -> anyhow::Result<()> {
    let contents = std::fs::read_to_string("input/day16").unwrap();

    let mut sections = contents.split("\n\n");

    let rules = aoclib::parse_lines(sections.next().unwrap(), parse_rule)?;
    let my_ticket: Ticket = parse_tickets(sections.next().unwrap())?
        .first()
        .cloned()
        .unwrap();
    let scanned_tickets = parse_tickets(sections.next().unwrap())?;

    let mut scanning_error_rate = 0;

    let mut valid_tickets = vec![];
    for ticket in scanned_tickets {
        let scanning_error = ticket_scanning_error(&ticket, &rules);
        scanning_error_rate += scanning_error;
        if scanning_error == 0 {
            valid_tickets.push(ticket);
        }
    }

    println!("Part 1 = {}", scanning_error_rate);

    let field_possibilities: Vec<HashSet<usize>> = valid_tickets
        .iter()
        .map(|ticket| possibilities(ticket, &rules))
        .fold_first(|a, x| {
            a.iter()
                .zip(x)
                .map(|(a, b)| a.intersection(&b).cloned().collect())
                .collect()
        })
        .unwrap();

    println!("Field possibilities = {:?}", field_possibilities);

    let mut solution = vec![];
    solve_for_order(&field_possibilities, &mut solution);
    println!("Field ordering: {:?}", solution);

    let solution: HashMap<usize, usize> =
        solution.iter().enumerate().map(|(i, x)| (*x, i)).collect();

    let mut part2 = 1;
    for (i, rule) in rules.iter().enumerate() {
        if rule.field_name.starts_with("departure") {
            let idx = solution.get(&i).unwrap();
            part2 *= my_ticket[*idx]
        }
    }

    println!("Part 2 = {}", part2);

    Ok(())
}
