use std::path::PathBuf;
use std::convert::Into;
use types::position::Position;
use types::{EvaluatedExpr,VarType};

// An error has a position and an underlying cause. Anything that
// can be turned into a SomeError (including this) can be wrapped in
// this struct, to build up a stack trace. It's intended that one
// creates an errror by running errors::new given a Position and an
// errors::Shape/Application etc, or a pre-existing Error.
#[derive(Clone,PartialEq,Debug)]
pub struct Error {
    position: Box<Position>,
    cause: Box<SomeError>
}

pub fn new<E: Into<SomeError>>(err: E, pos: Position) -> Error {
    Context{
        position: Box::new(pos),
        cause: Box::new(err.into())
    }
}

// Where an error happened.
#[derive(Clone,PartialEq,Debug)]
pub struct Position {
    pub start: usize,
    pub end: usize,
    pub file: PathBuf,
    pub function: String,
}

// An error falls into one of these categories,
// or can be a context which itself contains an
// error.
#[derive(Clone,PartialEq,Debug)]
enum SomeError {
    Application(Application),
    Import(Import),
    Shape(Shape),
    Syntax(Syntax),
    Context(Error)
}

impl From<Application> for SomeError {
    fn from(err: Application) -> Self {
        SomeError::Application(err)
    }
}
impl From<Import> for SomeError {
    fn from(err: Import) -> Self {
        SomeError::Import(err)
    }
}
impl From<Shape> for SomeError {
    fn from(err: Shape) -> Self {
        SomeError::Shape(err)
    }
}
impl From<Syntax> for SomeError {
    fn from(err: Syntax) -> Self {
        SomeError::Syntax(err)
    }
}
impl From<Error> for SomeError {
    fn from(err: Error) -> Self {
        SomeError::Context(err)
    }
}
impl From<Error> for SomeError {
    fn from(err: Error) -> Self {
        (*err.inner).into()
    }
}

// Errors applying functions.
#[derive(Clone,PartialEq,Debug)]
pub enum Application {
    CantFindVariable(String,VarType),
    NotAFunction,
    WrongNumberOfArguments{expected: usize, got: usize},
    WrongTypeOfArguments{index: usize, expected: String, got: String},
    PropertyDoesNotExist(String),
    UnitMismatch
}

// Errors importing things.
#[derive(Clone,PartialEq,Debug)]
pub enum Import {
    CannotImportNoPathSet
    CannotOpenFile(PathBuf),
    CannotReadFile(PathBuf),
    ImportLoop(Vec<PathBuf>, PathBuf),
    Import(Box<Error>),
    CycleDetected(Vec<String>)
}

// Errors with the shaoe of the formed CSS.
#[derive(Clone,PartialEq,Debug)]
pub enum Shape {
    KeyframesKeyvalsNotAllowedAtTop,
    KeyframesKeyframesBlockNotAllowed,
    KeyframesFontFaceBlockNotAllowed,
    KeyframesMediaBlockNotAllowed,
    KeyframesNestedBlockNotAllowed,
    FontfaceBlockNotAllowed,
    InvalidExpressionInCssValue(Box<EvaluatedExpr>),
    NotACSSBlock
}

// Errors parsing the syntax into an AST.
#[derive(Clone,PartialEq,Debug)]
pub enum Syntax {
    ParseError(String)
}
