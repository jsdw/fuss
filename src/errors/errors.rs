use std::path::{PathBuf};
use std::iter;
use std::convert::Into;
use types::{EvaluatedExpr,VarType,Kind,At,SmallVec};
use parser::parser::Rule;

// Usage:
// import errors::*;
// err(ApplicationError::NotAFunction, At::position(somefile, 0,100))
//
// An error has a position and an underlying cause. Anything that
// can be turned into a ErrorKind (including this) can be wrapped in
// this struct, to build up a stack trace. It's intended that one
// creates an errror by running errors::new given a Position and an
// errors::Shape/Application etc, or a pre-existing Error.
#[derive(Clone,PartialEq,Debug)]
pub struct Error {
    at: SmallVec<At>,
    cause: Box<ErrorKind>
}
impl ErrorText for Error {
    fn error_summary(&self) -> String {
        self.cause.error_summary()
    }
    fn error_description(&self) -> String {
        self.cause.error_description()
    }
}
impl Error {
    pub fn new<E: Into<ErrorKind>, L: Into<SmallVec<At>>>(err: E, pos: L) -> Error  {
        Error {
            at: pos.into(),
            cause: Box::new(err.into())
        }
    }
    pub fn at(&self) -> &At {
        self.at.last()
    }
    pub fn cause(&self) -> ErrorKind {
        (*self.cause).clone()
    }
}
pub fn err<E: Into<ErrorKind>, L: Into<SmallVec<At>>>(err: E, pos: L) -> Error {
    Error::new(err,pos)
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
    ContextError(ContextError)
}

impl ErrorText for ErrorKind {
    fn error_summary(&self) -> String {
        use self::ErrorKind::*;
        match *self {
            ApplicationError(ref e) => e.error_summary(),
            ImportError(ref e) => e.error_summary(),
            ShapeError(ref e) => e.error_summary(),
            SyntaxError(ref e) => e.error_summary(),
            ContextError(ref e) => e.error_summary()
        }
    }
    fn error_description(&self) -> String {
        use self::ErrorKind::*;
        match *self {
            ApplicationError(ref e) => e.error_description(),
            ImportError(ref e) => e.error_description(),
            ShapeError(ref e) => e.error_description(),
            SyntaxError(ref e) => e.error_description(),
            ContextError(ref e) => e.error_description()
        }
    }
}

impl <T> Into<Result<T,ErrorKind>> for ErrorKind {
    fn into(self: ErrorKind) -> Result<T,ErrorKind> {
        Err(self)
    }
}
impl <T> Into<Result<T,ErrorKind>> for ContextError {
    fn into(self: ContextError) -> Result<T,ErrorKind> {
        Err(self.into())
    }
}
impl <T> Into<Result<T,ErrorKind>> for ApplicationError {
    fn into(self: ApplicationError) -> Result<T,ErrorKind> {
        Err(self.into())
    }
}
impl <T> Into<Result<T,ErrorKind>> for ImportError {
    fn into(self: ImportError) -> Result<T,ErrorKind> {
        Err(self.into())
    }
}
impl <T> Into<Result<T,ErrorKind>> for ShapeError {
    fn into(self: ShapeError) -> Result<T,ErrorKind> {
        Err(self.into())
    }
}
impl <T> Into<Result<T,ErrorKind>> for SyntaxError {
    fn into(self: SyntaxError) -> Result<T,ErrorKind> {
        Err(self.into())
    }
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
impl From<ContextError> for ErrorKind {
    fn from(err: ContextError) -> Self {
        ErrorKind::ContextError(err)
    }
}

// Error context - wrap other errors for positional information
#[derive(Clone,PartialEq,Debug)]
pub enum ContextError {
    At(Error)
}

impl ErrorText for ContextError {
    fn error_summary(&self) -> String {
        use self::ContextError::*;
        match *self {
            At{..} => {
                "From".to_owned()
            }
        }
    }
    fn error_description(&self) -> String {
        String::new()
    }
}

// Errors applying functions.
#[derive(Clone,PartialEq,Debug)]
pub enum ApplicationError {
    CantFindVariable(String,VarType),
    NotAFunction,
    WrongNumberOfArguments{expected: usize, got: usize},
    WrongKindOfArguments{index: usize, expected: Vec<Kind>, got: Kind},
    WrongUnitOfArguments{index: usize, expected: Vec<String>, got: String},
    PropertyDoesNotExist(String),
    UnitMismatch,
    CycleDetected(Vec<String>, String)
}

impl ErrorText for ApplicationError {
    fn error_summary(&self) -> String {
        use self::ApplicationError::*;
        match *self {
            CantFindVariable{..} => {
                "I cannot find this variable".to_owned()
            },
            NotAFunction => {
                "This is not a function".to_owned()
            },
            WrongNumberOfArguments{..} => {
                "The wrong number of arguments are being used here".to_owned()
            },
            WrongKindOfArguments{..} => {
                "One or more of the arguments here are the wrong kind".to_owned()
            },
            WrongUnitOfArguments{..} => {
                "One or more of the arguments here have the wrong unit".to_owned()
            },
            PropertyDoesNotExist{..} => {
                "This property does not exist".to_owned()
            },
            UnitMismatch => {
                "Units do not match".to_owned()
            },
            CycleDetected{..} => {
                "A cycle has been detected".to_owned()
            }
        }
    }
    fn error_description(&self) -> String {
        use self::ApplicationError::*;
        match *self {
            CantFindVariable(ref name, ty) => {
                match ty {
                    VarType::User => format!("The variable '{}' has not been declared", name),
                    VarType::Builtin => format!("The built-in variable '{}' does not exist", name)
                }
            },
            NotAFunction => {
                format!("Trying to use something here as a function, but it is not")
            },
            WrongNumberOfArguments{expected,got} => {
                format!("This function expected {} arguments but got {}", expected, got)
            },
            WrongKindOfArguments{index, ref expected, got} => {
                let e = expected.into_iter().map(|k| k.to_string()).collect::<Vec<_>>().join(", ");
                format!("Argument {} is {}, but the function expected one of {}", index+1, got, e)
            },
            WrongUnitOfArguments{index, ref expected, ref got} => {
                let e = expected.join(", ");
                format!("Argument {} is a unit with type '{}', but the function expected one of {}", index+1, got, e)
            },
            PropertyDoesNotExist(ref prop) => {
                format!("trying to access the property '{}', which does not exist", prop)
            },
            UnitMismatch => {
                format!("the suffixes of the units need to match but they do not")
            },
            CycleDetected(ref vars, ref var) => {
                let cycle = vars
                    .into_iter()
                    .skip_while(|v| v != &var)
                    .chain(iter::once(var))
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(" => ");
                format!("Variables were caught accessing each other in a cycle:\n  {}", cycle)
            }
        }
    }
}

// Errors importing things.
#[derive(Clone,PartialEq,Debug)]
pub enum ImportError {
    CannotImportNoPathSet,
    CannotOpenFile(PathBuf),
    CannotReadFile(PathBuf),
    ImportLoop(Vec<PathBuf>, PathBuf),
    CompileError(Error, PathBuf)
}
impl ErrorText for ImportError {
    fn error_summary(&self) -> String {
        use self::ImportError::*;
        match *self {
            CannotImportNoPathSet => {
                "I cannot import things".to_owned()
            },
            CannotOpenFile{..} => {
                "I cannot open this file".to_owned()
            },
            CannotReadFile{..} => {
                "I cannot read this file".to_owned()
            },
            ImportLoop{..} => {
                "An import loop has been detected".to_owned()
            },
            CompileError{..} => {
                "This file was imported from".to_owned()
            }
        }
    }
    fn error_description(&self) -> String {
        use self::ImportError::*;
        match *self {
            CannotImportNoPathSet => {
                "I am working from standard in, and so 'import' cannot be used as I don't know where to look for things".to_owned()
            },
            CannotOpenFile{..} => {
                "Perhaps the path has been misspelt, or you do not have permission to access it?".to_owned()
            },
            CannotReadFile{..} => {
                "The file exists, but you may not have permission to read it".to_owned()
            },
            ImportLoop(ref paths, ref path) => {
                let cycle = paths
                    .into_iter()
                    .skip_while(|p| p != &path)
                    .chain(iter::once(path))
                    .map(|p| p.display().to_string())
                    .collect::<Vec<_>>()
                    .join("\n  ");
                format!("{}", cycle)
            },
            CompileError{..} => {
                String::new()
            }
        }
    }
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
    NakedKeyValNotAllowed,
    NotACSSBlock(Kind)
}
impl ErrorText for ShapeError {
    fn error_summary(&self) -> String {
        use self::ShapeError::*;
        match *self {
            KeyframesKeyvalsNotAllowedAtTop   |
            KeyframesKeyframesBlockNotAllowed |
            KeyframesFontFaceBlockNotAllowed  |
            KeyframesMediaBlockNotAllowed     |
            KeyframesNestedBlockNotAllowed => {
                "There is a problem in this @keyframes block".to_owned()
            },
            FontfaceBlockNotAllowed => {
                "There is a problem in this @font-face block".to_owned()
            },
            InvalidExpressionInCssValue{..} => {
                "Invalid value being inserted into this CSS".to_owned()
            },
            NakedKeyValNotAllowed => {
                "key:value pairs cannot be at the top level".to_owned()
            },
            NotACSSBlock{..} => {
                "This should be a CSS block".to_owned()
            }
        }
    }
    fn error_description(&self) -> String {
        use self::ShapeError::*;
        match *self {
            KeyframesKeyvalsNotAllowedAtTop => {
                format!("key:value pairs aren't allowed directly inside a @keyframes block")
            },
            KeyframesKeyframesBlockNotAllowed => {
                format!("@keyframes blocks aren't allowed inside other @keyframes blocks")
            },
            KeyframesFontFaceBlockNotAllowed => {
                format!("@font-face blocks aren't allowed inside @keyframes blocks")
            },
            KeyframesMediaBlockNotAllowed => {
                format!("@media blocks aren't allowed inside @keyframes blocks")
            },
            KeyframesNestedBlockNotAllowed => {
                format!("nested blocks aren't allowed inside @keyframes block sections")
            },
            FontfaceBlockNotAllowed => {
                format!("nested blocks aren't allowed inside @font-face blocks")
            },
            InvalidExpressionInCssValue(ref expr) => {
                format!("I can't interpolate an expression that's {} into a CSS value. I can interpolate strings, booleans, units and colors into CSS values", expr.kind())
            },
            NakedKeyValNotAllowed => {
                format!("Key:value pairs need to be inside a block with a selector")
            },
            NotACSSBlock(kind) => {
                format!("I expected a CSS block but got something that's {}", kind)
            }
        }
    }
}

// Errors parsing the syntax into an AST.
#[derive(Clone,PartialEq,Debug)]
pub enum SyntaxError {
    BadRule{ positives: Vec<Rule>, negatives: Vec<Rule> },
    Custom{ message: String }
}
impl ErrorText for SyntaxError {
    fn error_summary(&self) -> String {
        "There was an error parsing this".to_owned()
    }
    fn error_description(&self) -> String {
        use self::SyntaxError::*;
        match *self {
            BadRule{ positives: ref _positives, negatives: ref _negatives } => {
                format!("Unexpected rule")
            }
            Custom{ ref message } => {
                format!("{}", message)
            }
        }
    }
}

// A trait allowing errors to carry a summary and description:
pub trait ErrorText {
    fn error_summary(&self) -> String;
    fn error_description(&self) -> String;
}