use aoclib::{FromPair, Neighbours, Vec3, Vec4};
use std::collections::HashSet;
use std::hash::Hash;

fn parse_line(s: &str) -> anyhow::Result<HashSet<usize>> {
    let row = s
        .chars()
        .enumerate()
        .filter_map(|(i, x)| if x == '#' { Some(i) } else { None })
        .collect();
    Ok(row)
}

fn rows_to_grid<T>(rows: Vec<HashSet<usize>>) -> HashSet<T>
where
    T: Eq + FromPair + Hash,
{
    let mut result = HashSet::new();
    for (y, row) in rows.iter().enumerate() {
        for x in row {
            result.insert(T::from_pair(*x as i64, y as i64));
        }
    }
    result
}

fn is_alive(count: usize, was_alive: bool) -> bool {
    count == 3 || (count == 2 && was_alive)
}

fn neighbours<T>(cube: T) -> Vec<T>
where
    T: Neighbours,
{
    cube.neighbours()
}

fn main() -> anyhow::Result<()> {
    let rows = aoclib::read_parsed_lines("input/day17", parse_line)?;

    let conway: HashSet<Vec3<i64>> = rows_to_grid(rows.clone());
    let result = aoclib::run_life(conway, neighbours, is_alive, 6);
    println!("Part 1 = {}", result.len());

    let conway: HashSet<Vec4<i64>> = rows_to_grid(rows);
    let result = aoclib::run_life(conway, neighbours, is_alive, 6);
    println!("Part 2 = {}", result.len());

    Ok(())
}
