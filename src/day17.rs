use aoclib::{Vec4, Vec3, Neighbours, FromPair};
use std::collections::{HashMap, HashSet};
use std::ops::Add;
use std::hash::Hash;

fn parse_line(s: &str) -> anyhow::Result<HashSet<usize>> {
    let row = s
        .chars()
        .enumerate()
        .filter_map(|(i, x)| if x == '#' { Some(i) } else { None })
        .collect();
    Ok(row)
}

fn rows_to_grid<T>(rows: Vec<HashSet<usize>>) -> HashSet<T> where T: Eq + FromPair + Hash {
    let mut result = HashSet::new();
    for (y, row) in rows.iter().enumerate() {
        for x in row {
            result.insert(T::from_pair(*x as i64, y as i64));
        }
    }
    result
}

fn step<T>(input: &HashSet<T>) -> HashSet<T> where T: Add<Output=T> + Hash + Eq + Copy + Neighbours {
    let mut heat = HashMap::new();

    for cube in input {
        for v in cube.neighbours() {
            let new_x = match heat.get(&v) {
                None => 1,
                Some(x) => x + 1,
            };
            heat.insert(v, new_x);
        }
    }

    let mut result = HashSet::new();

    for (v, count) in heat {
        if count == 3 || (count == 2 && input.contains(&v)) {
            result.insert(v);
        }
    }
    result
}

fn main() -> anyhow::Result<()> {
    let rows = aoclib::read_parsed_lines("input/day17", parse_line)?;

    let conway : HashSet<Vec3<i64>> = rows_to_grid(rows.clone());
    let result = (0..6).fold(conway, |a, _i| step(&a));
    println!("Part 1 = {}", result.len());

    let conway : HashSet<Vec4<i64>> = rows_to_grid(rows);
    let result = (0..6).fold(conway, |a, _i| step(&a));
    println!("Part 2 = {}", result.len());

    Ok(())
}
