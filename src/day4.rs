#![feature(str_split_once)]

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

struct PassportIter<'a> {
    lines: std::str::Lines<'a>,
}

impl<'a> PassportIter<'a> {
    fn new(content: &'a str) -> Self {
        PassportIter {
            lines: content.lines(),
        }
    }
}

impl<'a> Iterator for PassportIter<'a> {
    type Item = Vec<(&'a str, &'a str)>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut fields: Self::Item = vec![];
        loop {
            match self.lines.next() {
                None | Some("") => {
                    if fields.is_empty() {
                        return None;
                    }
                    return Some(fields);
                }
                Some(text) => {
                    for item in text.split(" ") {
                        let (key, value) = item.split_once(":").unwrap();
                        fields.push((key, value))
                    }
                }
            }
        }
    }
}

const REQUIRED_FIELDS: &'static [&'static str] = &["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

lazy_static! {
    static ref HEIGHT_FORMAT: Regex = Regex::new(r"^(\d+)(cm|in)$").unwrap();
    static ref HAIR_COLOR: Regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
    static ref EYE_COLOR: Regex = Regex::new(r"^amb|blu|brn|gry|grn|hzl|oth$").unwrap();
    static ref PASSPORT_ID: Regex = Regex::new(r"^[0-9]{9}$").unwrap();
}

fn is_valid(key: &str, value: &str) -> bool {
    match (key, value) {
        ("byr", year) => is_valid_year(year, 1920, 2002),
        ("iyr", year) => is_valid_year(year, 2010, 2020),
        ("eyr", year) => is_valid_year(year, 2020, 2030),
        ("hgt", height) => is_valid_height(height),
        ("hcl", hcl) => HAIR_COLOR.is_match(hcl),
        ("ecl", ecl) => EYE_COLOR.is_match(ecl),
        ("pid", pid) => PASSPORT_ID.is_match(pid),
        ("cid", _) => true,
        (_, _) => false,
    }
}

fn is_valid_year(year: &str, min: i32, max: i32) -> bool {
    match year.parse::<i32>() {
        Ok(year) => year >= min && year <= max,
        _ => false,
    }
}

fn is_valid_height(height: &str) -> bool {
    if let Some(captures) = HEIGHT_FORMAT.captures(height) {
        let amount: i32 = captures.get(1).unwrap().as_str().parse().unwrap();
        match captures.get(2).unwrap().as_str() {
            "cm" => {
                return amount >= 150 && amount <= 193;
            }
            "in" => {
                return amount >= 59 && amount <= 76;
            }
            _ => {
                return false;
            }
        }
    }
    false
}

fn is_passport_valid(passport: &Vec<(&str, &str)>) -> bool {
    let passport_fields: HashMap<String, String> = passport
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();

    for field in REQUIRED_FIELDS {
        if !passport_fields.contains_key(&field.to_string()) {
            return false;
        }
    }

    for (key, value) in &passport_fields {
        if !is_valid(key.as_str(), value.as_str()) {
            return false;
        }
    }

    true
}

fn main() {
    let content = std::fs::read_to_string("input/day4").unwrap();

    let mut valid_passports = 0;
    for passport in PassportIter::new(&content) {
        if is_passport_valid(&passport) {
            valid_passports += 1;
        }
    }

    println!("Valid passports = {}", valid_passports);
}
