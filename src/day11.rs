use lazy_static::lazy_static;
use std::collections::HashMap;
use std::str::FromStr;

use aoclib::Vec2;

#[derive(PartialEq, Eq, Clone)]
struct Grid {
    seats: HashMap<Vec2<i64>, bool>,
    size: Vec2<i64>,
}

impl FromStr for Grid {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, anyhow::Error> {
        let mut seats = HashMap::new();

        let mut max_y = 0;
        let mut max_x = 0;

        for (y, row) in s.lines().enumerate() {
            for (x, seat) in row.chars().enumerate() {
                if let Some(occupied) = match seat {
                    'L' => Some(false),
                    '#' => Some(true),
                    '.' => None,
                    _ => return Err(anyhow::anyhow!("Unrecognised character: {}", seat)),
                } {
                    seats.insert(Vec2::new(x as i64, y as i64), occupied);
                }
                max_x = x;
            }
            max_y = y;
        }

        let size = Vec2::new(max_x as i64, max_y as i64);

        Ok(Grid { seats, size })
    }
}

lazy_static! {
    static ref NEIGHBOUR_OFFSETS: Vec<Vec2<i64>> = vec![
        Vec2::new(-1, -1),
        Vec2::new(-1, 0),
        Vec2::new(-1, 1),
        Vec2::new(0, -1),
        Vec2::new(0, 1),
        Vec2::new(1, -1),
        Vec2::new(1, 0),
        Vec2::new(1, 1)
    ];
}

impl Grid {
    fn update(&self) -> Self {
        self.map(|&seat, occupied| match self.occupied_neighbours(seat) {
            0 => true,
            x if x >= 4 => false,
            _ => occupied,
        })
    }

    fn update2(&self) -> Self {
        self.map(|&seat, occupied| match self.occupied_seen(seat) {
            0 => true,
            x if x >= 5 => false,
            _ => occupied,
        })
    }

    fn map<F>(&self, rule: F) -> Self
    where
        F: Fn(&Vec2<i64>, bool) -> bool,
    {
        let mut new_seats: HashMap<Vec2<i64>, bool> = HashMap::new();
        for (seat, occupied) in self.seats.iter() {
            new_seats.insert(seat.clone().clone(), rule(seat, occupied.clone()));
        }
        Grid {
            seats: new_seats,
            size: self.size,
        }
    }

    fn occupied_neighbours(&self, seat: Vec2<i64>) -> usize {
        NEIGHBOUR_OFFSETS
            .iter()
            .filter(|&offset| self.is_occupied(&(seat + offset.clone())))
            .count()
    }

    fn occupied_seen(&self, seat: Vec2<i64>) -> usize {
        NEIGHBOUR_OFFSETS
            .iter()
            .filter(|&offset| self.is_occupied_seen_in_direction(seat, offset.clone()))
            .count()
    }

    fn is_occupied_seen_in_direction(&self, seat: Vec2<i64>, offset: Vec2<i64>) -> bool {
        let mut seat = seat.clone();
        loop {
            seat = seat + offset;
            if seat.x < 0 || seat.y < 0 || seat.x > self.size.x || seat.y > self.size.y {
                return false;
            }

            match self.seats.get(&seat) {
                Some(true) => return true,
                Some(false) => return false,
                None => (),
            }
        }
    }

    fn is_occupied(&self, seat: &Vec2<i64>) -> bool {
        self.seats.get(seat).unwrap_or(&false).clone()
    }

    fn count_occupied(&self) -> usize {
        self.seats
            .iter()
            .filter(|(_k, v)| v.clone().clone())
            .count()
    }
}

fn part1(mut grid: Grid) {
    loop {
        let new_grid = grid.update();

        if new_grid == grid {
            break;
        }
        grid = new_grid;
    }

    println!("Part 1 = {}", grid.count_occupied());
}

fn part2(mut grid: Grid) {
    loop {
        let new_grid = grid.update2();

        if new_grid == grid {
            break;
        }
        grid = new_grid;
    }

    println!("Part 2 = {}", grid.count_occupied());
}

fn main() -> anyhow::Result<()> {
    let content = std::fs::read_to_string("input/day11")?;
    let grid = content.parse::<Grid>()?;

    part1(grid.clone());
    part2(grid.clone());

    Ok(())
}
