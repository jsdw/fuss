#[derive(Copy,Clone,Debug,PartialEq)]
pub struct Position(pub usize);

impl Position {
    pub fn new() -> Position {
        Position(0)
    }
}