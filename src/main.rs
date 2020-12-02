use regex::Regex;

fn char_count(text: &str, x: char) -> i64 {
    let mut count = 0;
    for character in text.chars() {
        if character == x {
            count+= 1;
        }
    }
    count
}

fn is_char_at(text: &str, needle: char, position: i64) -> bool {
    let char_at = text.chars().nth((position as usize - 1) as usize).unwrap();
    char_at == needle
}

fn main() {
    let content = std::fs::read_to_string("input/day2-1").unwrap();
    let password_line_format = Regex::new(r"^(\d+)-(\d+) (\w): (.+)$").unwrap();

    let mut valid_passwords_part1 = 0;
    let mut valid_passwords_part2 = 0;

    for line in content.lines() {
        println!("{}", line);
        let captures = password_line_format.captures(line).unwrap();
        let min : i64 = captures.get(1).unwrap().as_str().parse().unwrap();
        let max : i64 = captures.get(2).unwrap().as_str().parse().unwrap();
        let character = captures.get(3).unwrap().as_str().chars().next().unwrap();
        let password = captures.get(4).unwrap().as_str();

        let count = char_count(password, character);

        if count >= min && count <= max {
            valid_passwords_part1 += 1;
            println!("VALID");
        }

        let cond1 = is_char_at(password, character, min);
        let cond2 = is_char_at(password, character, max);
        if (cond1 || cond2) && !(cond1 && cond2) {
            valid_passwords_part2 += 1;
            println!("VALID 2");
        }
    }
    println!("Valid passwords (part 1) = {}", valid_passwords_part1);
    println!("Valid passwords (part 2) = {}", valid_passwords_part2);
}
