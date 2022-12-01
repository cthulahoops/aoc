fn is_ascending(password: &str) -> bool {
    let mut chars = password.chars();
    let mut last = chars.next().unwrap();
    for x in chars {
        if last > x {
            return false;
        }
        last = x;
    }
    true
}

fn runs(password: &str) -> Vec<(char, usize)> {
    let mut result = vec![];
    let mut chars = password.chars();
    let mut current = chars.next().unwrap();
    let mut count = 1;

    for x in chars {
        if x == current {
            count += 1;
        } else {
            result.push((current, count));
            current = x;
            count = 1;
        }
    }
    result.push((current, count));
    result
}

fn is_valid2(password: &str) -> bool {
    is_ascending(password) && runs(password).iter().any(|(_, c)| *c == 2)
}

fn is_valid1(password: &str) -> bool {
    let mut chars = password.chars();
    let mut last = chars.next().unwrap();
    let mut pair = false;
    for x in chars {
        if last == x {
            pair = true;
        }

        if last > x {
            return false;
        }

        last = x
    }
    pair
}

fn main() {
    let part1 = (172930..683082)
        .filter(|x| is_valid1(&format!("{}", x)))
        .count();

    println!("Part 1: {}", part1);

    let part1 = (172930..683082)
        .filter(|x| is_valid2(&format!("{}", x)))
        .count();
    println!("Part 2: {}", part1);
}

#[cfg(test)]
mod tests {
    use super::{is_valid1, is_valid2};

    #[test]
    fn test_is_valid1() {
        assert_eq!(is_valid1("111111"), true);
        assert_eq!(is_valid1("223450"), false);
        assert_eq!(is_valid1("123789"), false);
    }

    #[test]
    fn test_is_valid2() {
        assert_eq!(is_valid2("111111"), false);
        assert_eq!(is_valid2("223450"), false);
        assert_eq!(is_valid2("123789"), false);
        assert_eq!(is_valid2("123444"), false);
        assert_eq!(is_valid2("111122"), true);
    }
}
