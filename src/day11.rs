use lazy_static::lazy_static;

use aoclib::read_parsed_lines;
use aoclib::Vec2;

#[derive(PartialEq, Clone, Copy)]
enum Location {
    EmptySeat,
    OccupiedSeat,
    Floor,
}

#[derive(PartialEq, Clone)]
struct Grid {
    seats: Vec<Vec<Location>>,
    size: Vec2<i64>,
}

fn parse_line(s: &str) -> anyhow::Result<Vec<Location>> {
    s.chars()
        .map(|seat| match seat {
            'L' => Ok(Location::EmptySeat),
            '#' => Ok(Location::OccupiedSeat),
            '.' => Ok(Location::Floor),
            other => Err(anyhow::anyhow!("Unrecognised character: {}", other)),
        })
        .collect()
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
    fn new(seats: Vec<Vec<Location>>) -> Self {
        let size = Vec2::new(seats[0].len() as i64, seats.len() as i64);
        Grid { seats, size }
    }

    fn update(&self) -> Self {
        self.map(|seat, occupied| match self.occupied_neighbours(seat) {
            0 => true,
            x if x >= 4 => false,
            _ => occupied,
        })
    }

    fn update2(&self) -> Self {
        self.map(|seat, occupied| match self.occupied_seen(seat) {
            0 => true,
            x if x >= 5 => false,
            _ => occupied,
        })
    }

    fn map<F>(&self, rule: F) -> Self
    where
        F: Fn(Vec2<i64>, bool) -> bool,
    {
        let new_seats = self
            .seats
            .iter()
            .enumerate()
            .map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .map(|(x, location)| match location {
                        Location::Floor => Location::Floor,
                        other => {
                            if rule(
                                Vec2::new(x as i64, y as i64),
                                *other == Location::OccupiedSeat,
                            ) {
                                Location::OccupiedSeat
                            } else {
                                Location::EmptySeat
                            }
                        }
                    })
                    .collect()
            })
            .collect();

        Grid {
            seats: new_seats,
            size: self.size,
        }
    }

    fn occupied_neighbours(&self, seat: Vec2<i64>) -> usize {
        NEIGHBOUR_OFFSETS
            .iter()
            .filter(|&offset| self.is_occupied(seat + offset.clone()))
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
            if !self.in_bounds(seat) {
                return false;
            }

            match self.seats[seat.y as usize][seat.x as usize] {
                Location::OccupiedSeat => return true,
                Location::EmptySeat => return false,
                Location::Floor => continue,
            }
        }
    }

    fn in_bounds(&self, seat: Vec2<i64>) -> bool {
        seat.x >= 0 && seat.y >= 0 && seat.x < self.size.x && seat.y < self.size.y
    }

    fn is_occupied(&self, seat: Vec2<i64>) -> bool {
        self.in_bounds(seat)
            && self.seats[seat.y as usize][seat.x as usize] == Location::OccupiedSeat
    }

    fn count_occupied(&self) -> usize {
        self.seats
            .iter()
            .map(|row| row.iter().filter(|&l| *l == Location::OccupiedSeat).count())
            .sum()
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
    let seats = read_parsed_lines("input/day11", parse_line)?;
    let grid = Grid::new(seats);

    part1(grid.clone());
    part2(grid.clone());

    Ok(())
}
