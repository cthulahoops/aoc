use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashMap, HashSet};

lazy_static! {
    static ref LINE: Regex = Regex::new(r"^([^(]+) \(contains ([^)]+)\)").unwrap();
}

type Line = (HashSet<String>, Vec<String>);

fn parse_line(s: &str) -> anyhow::Result<Line> {
    let captures = LINE.captures(s).unwrap();

    let ingredients = captures
        .get(1)
        .unwrap()
        .as_str()
        .split(" ")
        .map(|x| x.to_string())
        .collect();
    let allergens = captures
        .get(2)
        .unwrap()
        .as_str()
        .split(", ")
        .map(|x| x.to_string())
        .collect();

    Ok((ingredients, allergens))
}

fn solve(possibilities: HashMap<String, HashSet<String>>) -> Option<Vec<(String, String)>> {
    let poss_vec: Vec<(String, HashSet<String>)> = possibilities.into_iter().collect();
    let mut used = HashSet::new();
    let mut solution = vec![];

    if do_solve(&poss_vec, &mut solution, &mut used) {
        Some(solution)
    } else {
        None
    }
}

fn do_solve(
    possibilities: &Vec<(String, HashSet<String>)>,
    solution: &mut Vec<(String, String)>,
    used: &mut HashSet<String>,
) -> bool {
    let position = solution.len();

    if position == possibilities.len() {
        return true;
    }

    let (ref allergen, ref words) = possibilities[position];

    for word in words {
        if used.contains(word) {
            continue;
        }

        solution.push((allergen.clone(), word.clone()));
        used.insert(word.clone());

        if do_solve(possibilities, solution, used) {
            return true;
        }
        used.remove(word);
        solution.pop();
    }

    return false;
}

fn main() -> anyhow::Result<()> {
    let data = aoclib::read_parsed_lines("input/day21", parse_line)?;

    let mut possibilities: HashMap<String, HashSet<String>> = HashMap::new();
    let mut all_ingredients: Vec<String> = vec![];

    for (ingredients, allergens) in data {
        for allergen in allergens {
            let possibile_ingredients = match possibilities.get(&allergen) {
                None => ingredients.clone(),
                Some(other_ingedients) => ingredients
                    .intersection(other_ingedients)
                    .cloned()
                    .collect(),
            };
            possibilities.insert(allergen, possibile_ingredients);
        }
        for item in ingredients {
            all_ingredients.push(item);
        }
    }

    println!("Possibilities: {:?}", possibilities);

    let mut allergens: HashSet<String> = HashSet::new();
    for (_k, v) in &possibilities {
        allergens = allergens.union(&v).cloned().collect();
    }

    println!("Allergens: {:?}", allergens);

    let mut safe = 0;
    for x in all_ingredients {
        if !allergens.contains(&x) {
            safe += 1;
        }
    }

    println!("Safe = {}", safe);

    let mut solution = solve(possibilities).expect("No solution");

    solution.sort();

    println!("Solved: {:?}", solution);

    let words: Vec<String> = solution.iter().map(|(_k, v)| v.clone()).collect();

    for word in words {
        print!("{},", word);
    }
    println!("");

    Ok(())
}
