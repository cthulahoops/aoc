use aoclib::{run_life, Vec2};
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashSet;

lazy_static! {
    static ref DIRECTION: Regex = Regex::new(r"nw|sw|ne|nw|se|w|e").unwrap();
    static ref HEX_NEIGHBOURS: Vec<Vec2<i64>> = parse_line("nwswwnesee").unwrap();
}

fn parse_line(s: &str) -> anyhow::Result<Vec<Vec2<i64>>> {
    let directions = DIRECTION
        .find_iter(s)
        .map(|x| match x.as_str() {
            "nw" => Vec2::new(0, -1),
            "sw" => Vec2::new(-1, 1),
            "w" => Vec2::new(-1, 0),
            "ne" => Vec2::new(1, -1),
            "se" => Vec2::new(0, 1),
            "e" => Vec2::new(1, 0),
            _ => panic!("Impossible!"),
        })
        .collect();
    Ok(directions)
}

fn hex_neighbours(v: Vec2<i64>) -> Vec<Vec2<i64>> {
    HEX_NEIGHBOURS.iter().map(|offset| *offset + v).collect()
}

fn is_black(count: usize, black: bool) -> bool {
    count == 2 || (count == 1 && black)
}

fn main() -> anyhow::Result<()> {
    let lines = aoclib::read_parsed_lines("input/day24", parse_line).unwrap();

    let mut black: HashSet<Vec2<i64>> = HashSet::new();

    for line in lines {
        let sum: Vec2<i64> = line.iter().fold(Vec2::new(0, 0), |a, b| a + *b);

        if black.contains(&sum) {
            black.remove(&sum);
        } else {
            black.insert(sum);
        }
    }
    println!("Part 1 = {}", black.len());

    let result = run_life(black, hex_neighbours, is_black, 100);

    println!("Part 2 = {}", result.len());
    Ok(())
}
