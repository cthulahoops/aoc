use lazy_static::lazy_static;
use regex::Regex;

fn char_count(text: &str, x: char) -> usize {
    text.chars().filter(|&character| character == x).count()
}

fn is_char_at(text: &str, needle: char, position: usize) -> bool {
    let char_at = text.chars().nth(position - 1).unwrap();
    char_at == needle
}

lazy_static! {
    static ref PASSWORD_LINE_FORMAT: Regex = Regex::new(r"^(\d+)-(\d+) (\w): (.+)$").unwrap();
}

struct Policy {
    min: usize,
    max: usize,
    character: char,
}

fn parse_line(line: &str) -> (Policy, &str) {
    let captures = PASSWORD_LINE_FORMAT.captures(line).unwrap();
    let min: usize = captures.get(1).unwrap().as_str().parse().unwrap();
    let max: usize = captures.get(2).unwrap().as_str().parse().unwrap();
    let character = captures.get(3).unwrap().as_str().chars().next().unwrap();
    let password = captures.get(4).unwrap().as_str();

    (
        Policy {
            min,
            max,
            character,
        },
        password,
    )
}

impl Policy {
    fn has_valid_count(&self, password: &str) -> bool {
        let count = char_count(password, self.character);
        count >= self.min && count <= self.max
    }

    fn has_valid_positional(&self, password: &str) -> bool {
        is_char_at(password, self.character, self.min)
            ^ is_char_at(password, self.character, self.max)
    }
}

fn main() {
    let content = std::fs::read_to_string("input/day2-1").unwrap();

    let mut valid_passwords_part1 = 0;
    let mut valid_passwords_part2 = 0;

    for line in content.lines() {
        let (policy, password) = parse_line(line);

        if policy.has_valid_count(password) {
            valid_passwords_part1 += 1;
        }

        if policy.has_valid_positional(password) {
            valid_passwords_part2 += 1;
        }
    }
    println!("Valid passwords (part 1) = {}", valid_passwords_part1);
    println!("Valid passwords (part 2) = {}", valid_passwords_part2);
}
