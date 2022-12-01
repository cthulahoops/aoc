#![feature(iterator_fold_self)]
use aoclib::vec::Vec2;
use std::collections::{HashMap, HashSet};

type Path = Vec<(i64, Vec2<i64>)>;

fn parse_path(s: &str) -> Path {
    s.split(",")
        .map(|word| {
            let mut chars = word.chars();
            let direction = match chars.next().unwrap() {
                'U' => Vec2::new(0, -1),
                'D' => Vec2::new(0, 1),
                'L' => Vec2::new(-1, 0),
                'R' => Vec2::new(1, 0),
                _ => panic!("Unknown direction"),
            };
            let length = chars.collect::<String>().parse::<i64>().unwrap();
            (length, direction)
        })
        .collect()
}

fn points(segments: Path) -> HashMap<Vec2<i64>, usize> {
    let mut result = HashMap::new();
    let mut position = Vec2::new(0, 0);
    let mut distance = 0;
    for (length, direction) in segments {
        for _ in 0..length {
            position = position + direction;
            distance += 1;
            result.insert(position, distance);
        }
    }
    result
}

type Crossings = HashMap<Vec2<i64>, (usize, usize)>;

fn find_crossings(a: Path, b: Path) -> Crossings {
    let points_a = points(a);
    let points_b = points(b);

    points_a
        .keys()
        .cloned()
        .collect::<HashSet<Vec2<i64>>>()
        .intersection(&points_b.keys().cloned().collect::<HashSet<Vec2<i64>>>())
        .map(|x| (x.clone(), (points_a[x], points_b[x])))
        .collect()
}

fn minimum<I, T>(a: I) -> T
where
    I: Iterator<Item = T>,
    T: PartialOrd,
{
    a.into_iter()
        .fold_first(|a, b| if a < b { a } else { b })
        .unwrap()
}

fn closest_crossing(crossings: Crossings) -> i64 {
    minimum(crossings.keys().map(|x| x.manhatten()))
}

fn fastest_crossing(crossings: Crossings) -> usize {
    minimum(crossings.values().map(|(a, b)| a + b))
}

fn main() {
    let common = find_crossings(
        parse_path("R75,D30,R83,U83,L12,D49,R71,U7,L72"),
        parse_path("U62,R66,U55,R34,D71,R55,D58,R83"),
    );

    println!("Common: {:?}", closest_crossing(common));

    let content = std::fs::read_to_string("input/day3").unwrap();
    let mut paths = content.lines().map(parse_path);

    let first = paths.next().unwrap();
    let second = paths.next().unwrap();

    let common = find_crossings(first, second);
    println!("Part 1 : {}", closest_crossing(common.clone()));
    println!("Part 2 : {}", fastest_crossing(common));
}
