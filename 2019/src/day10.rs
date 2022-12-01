use std::collections::HashMap;
use num::rational::Rational32;

type Map = Vec<Vec<char>>;
fn parse_map(s: &str) -> Map {
    s.lines().map(|x| x.chars().collect::<Vec<char>>()).collect()
}

fn seen(layout: Map) -> ((i32, i32), HashMap<(i32, Rational32), (i32, i32)>) {
    let mut best = 0;
    let mut best_at = (0, 0);
    let mut best_set = None;

    for (j, line) in layout.iter().enumerate() {
        for (i, x) in line.iter().enumerate() {
            if *x == '#' {
                let mut set = HashMap::new();
                for (j2, line) in layout.iter().enumerate() {
                    for (i2, y) in line.iter().enumerate() {
                        if *y == '#' {
                            let y = j2 as i32 - j as i32;
                            let x = i2 as i32 - i as i32;
                            let ratio = if y == 0 {
                                if x == 0 {
                                    continue;
                                }
                                Rational32::new(1_000_000 * x.signum(), 1)
                            }
                            else {
                                Rational32::new(x, y)
                            };

                            let key = (y.signum(), ratio);

                            match set.get(&key) {
                                Some((x0, y0)) if x0 + y0 > y + x => { set.insert(key, (x, y)); }
                                None => { set.insert(key, (x, y)); }
                                _ => {}
                            };

                            // let yf : f64 = ratio.denom().clone().into();
                            // let xf : f64 = ratio.numer().clone().into();

                            // set.insert(xf.atan2(yf))
                        }
                    }
                }
                if set.len() > best {
                    best = set.len();
                    best_at = (i as i32, j as i32);
                    best_set = Some(set);
                }
            }
        }
    }
    println!("Best at: {:?}", best_at);
    (best_at, best_set.unwrap())
}

fn targets(best: HashMap<(i32, Rational32), (i32, i32)>) -> Vec<(i32, i32)> {
    let mut targets : Vec<(i32, i32)> = best.values().cloned().collect();
    targets.sort_by(|(x0, y0), (x1, y1)| {
        let a0 = f64::from(*x0).atan2(f64::from(*y0));
        let a1 = f64::from(*x1).atan2(f64::from(*y1));
        a0.partial_cmp(&a1).unwrap()
    });
    targets
}

fn main() {
    let layout = parse_map(&std::fs::read_to_string("input/day10").unwrap());

    let (_best_at, best) = seen(layout);
    println!("Part 1: {}", best.len());

    let targets = targets(best);
    println!("Target: {:?}", targets[200 - 1]);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn count_seen(layout: Map) -> usize {
        let (_, best) = seen(layout);
        best.len()
    }

    #[test]
    fn test_example1() {
        let example = ".#..#\n....\n#####\n....#\n...##\n";
        assert_eq!(count_seen(parse_map(example)), 8);
    }

    #[test]
    fn test_example2() {
        let example = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####\n";
        assert_eq!(count_seen(parse_map(example)), 33);
    }

    #[test]
    fn test_example3() {
        let example = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##\n";
        assert_eq!(count_seen(parse_map(example)), 210);
    }

    #[test]
    fn test_part2() {
        let example = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##\n";

        let (best_at, best) = seen(parse_map(example));
        let targets = targets(best).iter().map(|(x, y)| (x + best_at.0, y + best_at.1)).collect::<Vec<(i32, i32)>>();

        println!("Targets: {:?}", targets);
        assert_eq!((11, 12), targets[0]);
        assert_eq!((12, 1), targets[1]);
    }
}
