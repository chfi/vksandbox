pub mod point;
pub mod shape;

pub use point::*;
pub use shape::*;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Space {
    width: usize,
    height: usize,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Rectangle {
    left: f32,
    right: f32,
    width: f32,
    height: f32,
}

impl Space {
    pub fn new(width: usize, height: usize) -> Self {
        Self { width, height }
    }

    pub fn dimensions_f32(&self) -> [f32; 2] {
        [self.width as f32, self.height as f32]
    }

    pub fn contains_f32(&self, x: f32, y: f32) -> bool {
        (0.0 <= x && x < (self.width as f32)) && (0.0 <= y && y < (self.height as f32))
    }
}
