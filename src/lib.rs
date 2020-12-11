use derive_more::Add;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Add)]
pub struct Vec2<T> {
    pub x: T,
    pub y: T,
}

impl<T> Vec2<T> {
    pub fn new(x: T, y: T) -> Self {
        Vec2 { x, y }
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
