use derive_more::{Add, Mul};
use lazy_static::lazy_static;

pub trait FromPair {
    fn from_pair(x: i64, y: i64) -> Self;
}

pub trait Neighbours {
    fn neighbours(&self) -> Vec<Self>
    where
        Self: Sized;
}

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

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Add, Mul)]
pub struct Vec3<T> {
    pub x: T,
    pub y: T,
    pub z: T,
}

impl<T> Vec3<T> {
    pub fn new(x: T, y: T, z: T) -> Self {
        Vec3 { x, y, z }
    }
}

impl FromPair for Vec3<i64> {
    fn from_pair(x: i64, y: i64) -> Self {
        Vec3::new(x, y, 0)
    }
}

fn offsets3() -> Vec<Vec3<i64>> {
    let mut results = vec![];
    for x in -1..2 {
        for y in -1..2 {
            for z in -1..2 {
                if x != 0 || y != 0 || z != 0 {
                    results.push(Vec3::new(x, y, z));
                }
            }
        }
    }
    results
}

fn offsets4() -> Vec<Vec4<i64>> {
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
    static ref NEIGHBOUR_OFFSETS_3: Vec<Vec3<i64>> = offsets3();
    static ref NEIGHBOUR_OFFSETS_4: Vec<Vec4<i64>> = offsets4();
}

impl Neighbours for Vec3<i64> {
    fn neighbours(&self) -> Vec<Self> {
        NEIGHBOUR_OFFSETS_3.iter().map(|x| *self + *x).collect()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Add, Mul)]
pub struct Vec4<T> {
    pub x: T,
    pub y: T,
    pub z: T,
    pub w: T,
}

impl<T> Vec4<T> {
    pub fn new(x: T, y: T, z: T, w: T) -> Self {
        Vec4 { x, y, z, w }
    }
}

impl FromPair for Vec4<i64> {
    fn from_pair(x: i64, y: i64) -> Self {
        Vec4::new(x, y, 0, 0)
    }
}

impl Neighbours for Vec4<i64> {
    fn neighbours(&self) -> Vec<Self> {
        NEIGHBOUR_OFFSETS_4.iter().map(|x| *self + *x).collect()
    }
}

pub fn parse_lines<F, R>(s: &str, parse: F) -> anyhow::Result<Vec<R>>
where
    F: Fn(&str) -> anyhow::Result<R>,
{
    s.lines().map(parse).collect()
}
