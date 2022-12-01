use std::collections::HashSet;
use std::num::ParseIntError;
use std::str::FromStr;

use aoclib::Vec2;

struct Map {
    height: usize,
    width: usize,
    trees: HashSet<Vec2<usize>>,
}

impl Map {
    fn has_tree(&self, point: Vec2<usize>) -> bool {
        self.trees.contains(&point.rem_x(self.width))
    }

    fn attempt_run(&self, velocity: Vec2<usize>) -> i64 {
        let mut position = Vec2 { x: 0, y: 0 };
        let mut painful_collisions = 0;

        while position.y <= self.height {
            if self.has_tree(position) {
                painful_collisions += 1;
            }
            position = position + velocity;
        }

        painful_collisions
    }
}

impl FromStr for Map {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut trees = HashSet::new();
        let mut width = 0;
        let mut height = 0;
        for (y, line) in s.lines().enumerate() {
            for (x, item) in line.chars().enumerate() {
                if item == '#' {
                    trees.insert(Vec2 { x, y });
                }
            }
            width = line.len();
            height = y;
        }
        Ok(Map {
            width,
            height,
            trees,
        })
    }
}

fn main() {
    let content = std::fs::read_to_string("input/day3").unwrap();

    let map = content.parse::<Map>().unwrap();

    let slope_options = vec![
        Vec2 { x: 1, y: 1 },
        Vec2 { x: 3, y: 1 },
        Vec2 { x: 5, y: 1 },
        Vec2 { x: 7, y: 1 },
        Vec2 { x: 1, y: 2 },
    ];

    let mut tree_product: i64 = 1;
    for velocity in slope_options {
        let painful_collisions = map.attempt_run(velocity);
        println!("{:?} We hit {} trees. Ouch", velocity, painful_collisions);
        tree_product *= painful_collisions;
    }
    println!("Total pain: {}", tree_product);
}
