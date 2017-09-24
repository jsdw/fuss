use std::path::PathBuf;
use types::position::Position;
use types::EvaluatedExpr;

/// Error types
#[derive(Clone,PartialEq,Debug)]
pub struct Error {
    pub ty: ErrorType,
    pub file: PathBuf,
    pub start: Position,
    pub end: Position
}

#[derive(Clone,PartialEq,Debug)]
pub enum ErrorType {
    NotAValidInnerBlock,
    CantFindVariable(String),
    NotAFunction,
    WrongNumberOfArguments{expected: usize, got: usize},
    WrongTypeOfArguments{message: String},
    NotACSSBlock,
    PropertyDoesNotExist(String),
    InvalidExpressionInCssValue(Box<EvaluatedExpr>),
    UnitMismatch,
    CannotOpenFile(PathBuf),
    CannotReadFile(PathBuf),
    CannotImportNoPathSet,

    ImportLoop(Vec<PathBuf>, PathBuf),
    ImportError(Box<Error>),
    CycleDetected(Vec<String>),

    // outputter errors:
    KeyframesKeyvalsNotAllowedAtTop,
    KeyframesKeyframesBlockNotAllowed,
    KeyframesFontFaceBlockNotAllowed,
    KeyframesMediaBlockNotAllowed,
    KeyframesNestedBlockNotAllowed,
    FontfaceBlockNotAllowed
}