use std::path::PathBuf;
use std::convert::Into;
use types::{EvaluatedExpr,VarType,Kind};

// Usage:
// import errors::*;
// Error::new(ApplicationError::NotAFunction, Location::at(0,100))
//
// An error has a position and an underlying cause. Anything that
// can be turned into a ErrorKind (including this) can be wrapped in
// this struct, to build up a stack trace. It's intended that one
// creates an errror by running errors::new given a Position and an
// errors::Shape/Application etc, or a pre-existing Error.
#[derive(Clone,PartialEq,Debug)]
pub struct Error {
    location: Box<Location>,
    cause: Box<ErrorKind>
}

impl Error {
    pub fn new<E: Into<ErrorKind>>(err: E, pos: Location) -> Error {
        Error {
            location: Box::new(pos),
            cause: Box::new(err.into())
        }
    }
}
pub fn err<E: Into<ErrorKind>>(err: E, pos: Location) -> Error {
    Error::new(err.into(),pos)
}

// Where an error happened.
#[derive(Clone,PartialEq,Debug)]
pub struct Location {
    start: usize,
    end: usize,
    file: PathBuf,
    function: String,
}

impl Location {
    pub fn at(start: usize, end: usize) -> Location {
        Location {
            start: start,
            end: end,
            file: PathBuf::new(),
            function: String::new()
        }
    }
    pub fn file(mut self, f: PathBuf) -> Location {
        self.file = f;
        self
    }
    pub fn func(mut self, f: String) -> Location {
        self.function = f;
        self
    }
}

// An error falls into one of these categories,
// or can be a context which itself contains an
// error.
#[derive(Clone,PartialEq,Debug)]
pub enum ErrorKind {
    ApplicationError(ApplicationError),
    ImportError(ImportError),
    ShapeError(ShapeError),
    SyntaxError(SyntaxError),
    Context(Error)
}

impl From<ApplicationError> for ErrorKind {
    fn from(err: ApplicationError) -> Self {
        ErrorKind::ApplicationError(err)
    }
}
impl From<ImportError> for ErrorKind {
    fn from(err: ImportError) -> Self {
        ErrorKind::ImportError(err)
    }
}
impl From<ShapeError> for ErrorKind {
    fn from(err: ShapeError) -> Self {
        ErrorKind::ShapeError(err)
    }
}
impl From<SyntaxError> for ErrorKind {
    fn from(err: SyntaxError) -> Self {
        ErrorKind::SyntaxError(err)
    }
}
impl From<Error> for ErrorKind {
    fn from(err: Error) -> Self {
        ErrorKind::Context(err)
    }
}

// Errors applying functions.
#[derive(Clone,PartialEq,Debug)]
pub enum ApplicationError {
    CantFindVariable(String,VarType),
    NotAFunction,
    WrongNumberOfArguments{expected: usize, got: usize},
    WrongKindOfArguments{index: usize, expected: Vec<Kind>, got: Kind},
    PropertyDoesNotExist(String),
    UnitMismatch
}

// Errors importing things.
#[derive(Clone,PartialEq,Debug)]
pub enum ImportError {
    CannotImportNoPathSet,
    CannotOpenFile(PathBuf),
    CannotReadFile(PathBuf),
    ImportLoop(Vec<PathBuf>, PathBuf),
    Import(Box<Error>),
    CycleDetected(Vec<String>)
}

// Errors with the shaoe of the formed CSS.
#[derive(Clone,PartialEq,Debug)]
pub enum ShapeError {
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
pub enum SyntaxError {
    ParseError(String)
}
