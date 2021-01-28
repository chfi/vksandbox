use super::Point;

pub trait HasPos {
    fn pos(&self) -> Point;

    fn translate(&mut self, delta: Point);
}

pub trait Shape: HasPos {
    fn contains_point(&self, p: Point) -> bool;
}

pub trait SDF: Shape {
    fn point_dist(&self, p: Point) -> f32;
}

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Circle {
    pos: Point,
    radius: f32,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Line {
    start: Point,
    end: Point,
    width: f32,
}

/*
   Circle impls
*/

impl Circle {
    pub fn new(pos: Point, radius: f32) -> Self {
        Self { pos, radius }
    }
}

impl Shape for Circle {
    #[inline]
    fn contains_point(&self, p: Point) -> bool {
        let rad_sqr = self.radius.powi(2);
        let dist_sqr = self.pos.dist_sqr(p);
        dist_sqr <= rad_sqr
    }
}

impl SDF for Circle {
    #[inline]
    fn point_dist(&self, p: Point) -> f32 {
        let dist_origin = self.pos.dist(p);
        let dist_border = dist_origin - self.radius;
        dist_border
    }
}

/*
  Line impls
*/

impl Line {
    pub fn new(start: Point, end: Point, width: f32) -> Self {
        Self { start, end, width }
    }

    #[inline]
    pub fn dist_to_point(&self, p: Point) -> f32 {
        let x_diff = self.end.x - self.start.x;
        let y_diff = self.end.y - self.start.y;

        let v1 = x_diff * (self.start.y - p.y);
        let v2 = y_diff * (self.start.x - p.x);

        let nom = (v1 - v2).abs();
        let denom = (x_diff.powi(2) + y_diff.powi(2)).sqrt();
        nom / denom
    }
}

impl HasPos for Line {
    #[inline]
    fn pos(&self) -> Point {
        self.start
    }

    #[inline]
    fn translate(&mut self, delta: Point) {
        self.start += delta;
        self.end += delta;
    }
}

impl Shape for Line {
    #[inline]
    fn contains_point(&self, p: Point) -> bool {
        let dist = self.dist_to_point(p);
        dist < self.width
    }
}

impl SDF for Line {
    #[inline]
    fn point_dist(&self, p: Point) -> f32 {
        let dist_origin = self.dist_to_point(p);
        let dist_border = dist_origin - self.width;
        dist_border
    }
}

/*
   Macro & macro impls
*/

macro_rules! impl_has_pos {
    ($for:ident, $field:ident) => {
        impl HasPos for $for {
            #[inline]
            fn pos(&self) -> Point {
                self.$field
            }

            #[inline]
            fn translate(&mut self, delta: Point) {
                self.$field += delta;
            }
        }
    };
}

impl_has_pos!(Circle, pos);
