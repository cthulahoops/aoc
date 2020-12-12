use aoclib::Vec2;
use std::str::FromStr;

#[derive(Clone, Debug)]
enum Instruction {
    North,
    South,
    East,
    West,
    Forward,
    Left,
    Right,
}

impl FromStr for Instruction {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "N" => Ok(Instruction::North),
            "S" => Ok(Instruction::South),
            "E" => Ok(Instruction::East),
            "W" => Ok(Instruction::West),
            "F" => Ok(Instruction::Forward),
            "L" => Ok(Instruction::Left),
            "R" => Ok(Instruction::Right),
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
    match degrees_clockwise {
        0 => vec,
        90 => Vec2::new(-vec.y, vec.x),
        180 => vec * -1,
        270 => Vec2::new(vec.y, -vec.x),
        _ => panic!("Unexpected number of degrees: {}", degrees_clockwise)
    }
}

fn main() -> anyhow::Result<()> {
    let commands = aoclib::read_parsed_lines("input/day12", parse_line)?;

    let mut position: Vec2<i64> = Vec2::new(0, 0);
    let mut direction: Vec2<i64> = Vec2::new(1, 0);

    for (instruction, value) in &commands {
        match instruction {
            Instruction::North => position = position + Vec2::new(0, -1) * value,
            Instruction::South => position = position + Vec2::new(0, 1) * value,
            Instruction::West => position = position + Vec2::new(-1, 0) * value,
            Instruction::East => position = position + Vec2::new(1, 0) * value,
            Instruction::Forward => position = position + direction * value,
            Instruction::Left => direction = rotate(direction, 360 - value),
            Instruction::Right => direction = rotate(direction, *value),
        }
    }

    println!("Part 1 : Distance: {}", position.manhatten());

    let mut position: Vec2<i64> = Vec2::new(0, 0);
    let mut waypoint: Vec2<i64> = Vec2::new(10, -1); 

    for (instruction, value) in &commands {
        match instruction {
            Instruction::North => waypoint = waypoint + Vec2::new(0, -1) * value,
            Instruction::South => waypoint = waypoint + Vec2::new(0, 1) * value,
            Instruction::West => waypoint = waypoint + Vec2::new(-1, 0) * value,
            Instruction::East => waypoint = waypoint + Vec2::new(1, 0) * value,
            Instruction::Forward => position = position + waypoint * value,
            Instruction::Left => waypoint = rotate(waypoint, 360 - value),
            Instruction::Right => waypoint = rotate(waypoint, *value),
        }
    }

    println!("Part 2 : Distance: {}", position.manhatten());


    Ok(())
}
