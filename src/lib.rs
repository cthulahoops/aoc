use derive_more::{Add, Mul};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Add, Mul)]
pub struct Vec2<T> {
    pub x: T,
    pub y: T,
}

impl<T> Vec2<T> {
    pub fn new(x: T, y: T) -> Self {
        Vec2 { x, y }
    }
}

impl Vec2<i64> {
    pub fn manhatten(self) -> i64 {
        self.x.abs() + self.y.abs()
    }
}

impl Vec2<usize> {
    pub fn rem_x(self, rhs: usize) -> Self {
        Vec2 {
            x: self.x % rhs,
            y: self.y,
        }
    }
}

pub fn parse_lines<F, R>(s: &str, parse: F) -> anyhow::Result<Vec<R>>
where
    F: Fn(&str) -> anyhow::Result<R>,
{
    s.lines().map(parse).collect()
}

pub fn read_parsed_lines<F, R>(filename: &str, parse: F) -> anyhow::Result<Vec<R>>
where
    F: Fn(&str) -> anyhow::Result<R>,
{
    let content = std::fs::read_to_string(filename)?;
    parse_lines(&content, parse)
}

pub fn read_numbers(filename: &str) -> anyhow::Result<Vec<i64>> {
    read_parsed_lines(filename, |x| Ok(x.parse().unwrap()))
}
