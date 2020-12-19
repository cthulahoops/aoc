use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
enum Token {
    Rule(usize),
    Char(char),
    Pipe,
    Plus,
}

type Rule = Vec<Token>;
type RuleSet = HashMap<usize, Rule>;

fn expand_rule(rule: &Rule, ruleset: &RuleSet) -> String {
    let contents: String = rule
        .iter()
        .map(|x| match x {
            Token::Pipe => "|".to_string(),
            Token::Plus => "+".to_string(),
            Token::Char(c) => c.to_string(),
            Token::Rule(id) => expand_rule(ruleset.get(id).unwrap(), ruleset),
        })
        .collect();

    if contents.contains("|") {
        format!("({})", contents)
    } else {
        contents
    }
}

fn parse_token(s: &str) -> Token {
    if s == "|" {
        return Token::Pipe;
    }
    if s == "+" {
        return Token::Plus;
    }
    if let Ok(id) = s.parse::<usize>() {
        return Token::Rule(id);
    }

    Token::Char(s.chars().nth(1).unwrap())
}

fn parse_rule(s: &str) -> (usize, Rule) {
    let mut parts = s.split(": ");
    let id = parts.next().unwrap().parse::<usize>().unwrap();
    let tokens = parts.next().unwrap().split(" ").map(parse_token).collect();

    (id, tokens)
}

fn compile_regex(rules: &RuleSet) -> Regex {
    let expanded: String = format!("^{}$", expand_rule(&rules[&0], &rules));
    // println!("Expanded: {}", expanded);
    Regex::new(&expanded).unwrap()
}

fn count_matches(regex: Regex, inputs: &Vec<String>) -> usize {
    inputs.iter().filter(|x| regex.is_match(x)).count()
}

fn patch_rule(rules: &mut RuleSet, s: &str) {
    let (k, v) = parse_rule(s);
    rules.insert(k, v);
}

fn main() -> anyhow::Result<()> {
    let content = std::fs::read_to_string("input/day19")?;
    let mut sections = content.split("\n\n");

    let mut rules: RuleSet = sections.next().unwrap().lines().map(parse_rule).collect();
    let inputs: Vec<String> = sections
        .next()
        .unwrap()
        .lines()
        .map(|x| x.to_string())
        .collect();

    let regex = compile_regex(&rules);
    println!("Part 1 = {}", count_matches(regex, &inputs));

    // Patch the rules!
    patch_rule(&mut rules, "8: 42 +");
    patch_rule(
        &mut rules,
        "11: 42 31 | 42 42 31 31 | 42 42 42 31 31 31 | 42 42 42 42 31 31 31 31",
    );

    let regex = compile_regex(&rules);
    println!("Part 2 = {}", count_matches(regex, &inputs));

    Ok(())
}
