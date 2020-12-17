use aoclib::Vec4;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};

type Offsets = Vec<Vec4<i64>>;

fn offsets3() -> Offsets {
    let mut results = vec![];
    for x in -1..2 {
        for y in -1..2 {
            for z in -1..2 {
                if x != 0 || y != 0 || z != 0 {
                    results.push(Vec4::new(x, y, z, 0));
                }
            }
        }
    }
    results
}

fn offsets4() -> Offsets {
    let mut results = vec![];
    for x in -1..2 {
        for y in -1..2 {
            for z in -1..2 {
                for w in -1..2 {
                    if x != 0 || y != 0 || z != 0 || w != 0 {
                        results.push(Vec4::new(x, y, z, w));
                    }
                }
            }
        }
    }
    results
}

lazy_static! {
    static ref NEIGHBOUR_OFFSETS_3: Offsets = offsets3();
    static ref NEIGHBOUR_OFFSETS_4: Offsets = offsets4();
}

fn parse_line(s: &str) -> anyhow::Result<HashSet<usize>> {
    let row = s
        .chars()
        .enumerate()
        .filter_map(|(i, x)| if x == '#' { Some(i) } else { None })
        .collect();
    Ok(row)
}

fn rows_to_grid(rows: Vec<HashSet<usize>>) -> Conway {
    let mut result = HashSet::new();
    for (y, row) in rows.iter().enumerate() {
        for x in row {
            result.insert(Vec4::new(*x as i64, y as i64, 0, 0));
        }
    }
    result
}

type Conway = HashSet<Vec4<i64>>;

fn step(input: &Conway, offsets: &Offsets) -> Conway {
    let mut heat: HashMap<Vec4<i64>, usize> = HashMap::new();

    for cube in input {
        for offset in offsets.iter() {
            let v = *cube + *offset;
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
    let conway = rows_to_grid(rows);

    let result = (0..6).fold(conway.clone(), |a, _i| step(&a, &NEIGHBOUR_OFFSETS_3));
    println!("Part 1 = {}", result.len());

    let result = (0..6).fold(conway, |a, _i| step(&a, &NEIGHBOUR_OFFSETS_4));
    println!("Part 2 = {}", result.len());

    Ok(())
}
