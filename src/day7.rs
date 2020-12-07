use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::num::ParseIntError;
use std::str::FromStr;

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

struct Ruleset {
    required: HashMap<Color, Vec<(usize, Color)>>,
    permitted: HashMap<Color, HashSet<Color>>,
}

impl Ruleset {
    fn new() -> Self {
        Ruleset {
            required: HashMap::new(),
            permitted: HashMap::new(),
        }
    }

    fn add_rule(&mut self, rule: Rule) {
        for (_count, color) in &rule.contains {
            self.add_permission(color, &rule.color);
            self.required
                .insert(rule.color.clone(), rule.contains.clone());
        }
    }

    fn add_permission(&mut self, contained: &Color, container: &Color) {
        match self.permitted.get_mut(contained) {
            Some(v) => {
                v.insert(container.clone());
            }
            None => {
                let mut v = HashSet::new();
                v.insert(container.clone());
                self.permitted.insert(contained.clone(), v);
            }
        }
    }

    fn allowed_containers(&self, color: &str) -> HashSet<Color> {
        let mut allowed = HashSet::new();
        if let Some(rules) = self.permitted.get(color) {
            for container in rules {
                allowed.insert(container.clone());
                allowed = allowed
                    .union(&self.allowed_containers(container))
                    .cloned()
                    .collect();
            }
        }
        allowed
    }

    fn required_contents(&self, color: &str) -> usize {
        match self.required.get(color) {
            Some(contents) => contents
                .iter()
                .map(|(count, color)| count * (1 + self.required_contents(color)))
                .sum(),
            None => 0,
        }
    }
}

impl FromStr for Ruleset {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut ruleset = Self::new();
        for line in s.lines() {
            let rule = line.parse::<Rule>()?;
            ruleset.add_rule(rule)
        }
        Ok(ruleset)
    }
}

fn main() {
    let content = std::fs::read_to_string("input/day7").unwrap();
    let ruleset: Ruleset = content.parse::<Ruleset>().unwrap();

    println!(
        "Shiny gold: {}",
        ruleset.allowed_containers("shiny gold").len()
    );
    println!(
        "My bag must contain: {}",
        ruleset.required_contents("shiny gold")
    )
}
