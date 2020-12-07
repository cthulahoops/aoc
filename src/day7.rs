use lazy_static::lazy_static;
use regex::Regex;
use std::num::ParseIntError;
use std::str::FromStr;
use std::collections::{HashMap, HashSet};

type Color = String;

#[derive(Debug)]
struct Rule {
    color: Color,
    contains: Vec<(usize, Color)>,
}

lazy_static! {
    static ref RULE_FORMAT: Regex =
        Regex::new(r"^([a-z ]+) bags? contain (([0-9a-z ]+ bags?[,.] ?)+)$").unwrap();
    static ref CONTENT_FORMAT: Regex = Regex::new(r"([0-9]+) ([a-z ]+) bags?").unwrap();
}

impl FromStr for Rule {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let captures = RULE_FORMAT.captures(s).unwrap();

        let contents_str = captures.get(2).unwrap().as_str();

        let contains = CONTENT_FORMAT
            .captures_iter(contents_str)
            .map(|captures| {
                (
                    captures.get(1).unwrap().as_str().parse::<usize>().unwrap(),
                    captures.get(2).unwrap().as_str().to_string(),
                )
            })
            .collect();

        Ok(Rule {
            color: captures.get(1).unwrap().as_str().to_string(),
            contains: contains,
        })
    }
}

type PermittedContainers = HashMap<Color, HashSet<Color>>;

fn add_permission(permitted: &mut PermittedContainers, contained: &Color, container: &Color) {
    match permitted.get_mut(contained) {
        Some(v) => {
            v.insert(container.clone());
        }
        None => {
            let mut v = HashSet::new();
            v.insert(container.clone());
            permitted.insert(contained.clone(), v);
        }
    }
}

fn allowed_containers(permitted: &PermittedContainers, color: &Color) -> HashSet<Color> {
    let mut allowed = HashSet::new();
    if let Some(rules) = permitted.get(color) {
        for container in rules {
            allowed.insert(container.clone());
            allowed = allowed.union(&allowed_containers(permitted, container)).cloned().collect();
        }
    }
    allowed
}

type Ruleset = HashMap<Color, Vec<(usize, Color)>>;

fn required_contents(ruleset: &Ruleset, color: &Color) -> usize {
    match ruleset.get(color) {
        Some(contents) => {
            contents.iter().map(|(count, color)| count * (1 + required_contents(ruleset, color))).sum()
        }
        None =>
            0
    }
}

fn main() {
    let content = std::fs::read_to_string("input/day7").unwrap();
    let mut permitted : PermittedContainers = HashMap::new();
    let mut ruleset : Ruleset = HashMap::new();

    for line in content.lines() {
        let rule = line.parse::<Rule>().unwrap();
        for (_count, color) in &rule.contains {
            add_permission(&mut permitted, color, &rule.color);
            ruleset.insert(rule.color.clone(), rule.contains.clone());
        }
    }

    println!("Shiny gold: {}", allowed_containers(&permitted, &"shiny gold".to_string()).len()); 
    println!("My bag must contain: {}", required_contents(&ruleset, &"shiny gold".to_string()))
}
