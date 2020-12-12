use aoclib::Vec2;
use std::str::FromStr;

#[derive(Clone, Debug)]
enum Instruction {
    Compass(Vec2<i64>),
    Forward,
    Turn(i64),
}

impl FromStr for Instruction {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "N" => Ok(Instruction::Compass(Vec2::new(0, -1))),
            "S" => Ok(Instruction::Compass(Vec2::new(0, 1))),
            "W" => Ok(Instruction::Compass(Vec2::new(-1, 0))),
            "E" => Ok(Instruction::Compass(Vec2::new(1,0))),
            "F" => Ok(Instruction::Forward),
            "L" => Ok(Instruction::Turn(-1)),
            "R" => Ok(Instruction::Turn(1)),
            _ => Err(anyhow::anyhow!("Invalid instruction: {:?}", s)),
        }
    }
}

fn parse_line(s: &str) -> anyhow::Result<(Instruction, i64)> {
    let mut chars = s.chars();

    let instruction = chars.next().unwrap().to_string().parse::<Instruction>()?;
    let value = chars
        .collect::<String>()
        .parse::<i64>()
        .expect("Value not an integer");

    Ok((instruction, value))
}

fn rotate(vec: Vec2<i64>, degrees_clockwise: i64) -> Vec2<i64> {
    match degrees_clockwise % 360 {
        0 => vec,
        90 => Vec2::new(-vec.y, vec.x),
        180 => vec * -1,
        270 => Vec2::new(vec.y, -vec.x),
        x if x < 0 => rotate(vec, x + 360),
        _ => panic!("Unexpected number of degrees: {}", degrees_clockwise),
    }
}

struct Boat {
    position: Vec2<i64>,
    direction: Vec2<i64>,
}

fn part1(
    Boat {
        position,
        direction,
    }: Boat,
    command: &(Instruction, i64),
) -> Boat {
    let (instruction, value) = command;
    match instruction {
        Instruction::Compass(v) => Boat {
            position: position + *v * value,
            direction,
        },
        Instruction::Forward => Boat {
            position: position + direction * value,
            direction,
        },
        Instruction::Turn(dir) => Boat {
            position,
            direction: rotate(direction, value * dir),
        },
    }
}

fn part2(
    Boat {
        position,
        direction,
    }: Boat,
    command: &(Instruction, i64),
) -> Boat {
    let (instruction, value) = command;
    match instruction {
        Instruction::Compass(v) => Boat {
            position,
            direction: direction + *v * value,
        },
        Instruction::Forward => Boat {
            position: position + direction * value,
            direction,
        },
        Instruction::Turn(dir) => Boat {
            position,
            direction: rotate(direction, *dir * value),
        },
    }
}

fn main() -> anyhow::Result<()> {
    let commands = aoclib::read_parsed_lines("input/day12", parse_line)?;

    let boat = commands.iter().fold(
        Boat {
            position: Vec2::new(0, 0),
            direction: Vec2::new(1, 0),
        },
        part1,
    );

    println!("Part 1 : Distance: {}", boat.position.manhatten());

    let boat = commands.iter().fold(
        Boat {
            position: Vec2::new(0, 0),
            direction: Vec2::new(10, -1),
        },
        part2,
    );

    println!("Part 2 : Distance: {}", boat.position.manhatten());

    Ok(())
}
