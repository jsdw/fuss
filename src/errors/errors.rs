use std::path::PathBuf;
use std::fmt;
use std::iter;
use std::convert::Into;
use types::{EvaluatedExpr,VarType,Kind};

// Usage:
// import errors::*;
// err(ApplicationError::NotAFunction, Location::at(0,100))
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
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.cause.fmt(f)
        // TODO: we need to make use of location information added
        // to errors.
    }
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
impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ErrorKind::*;

        match *self {
            ApplicationError(ref e) => e.fmt(f),
            ImportError(ref e) => e.fmt(f),
            ShapeError(ref e) => e.fmt(f),
            SyntaxError(ref e) => e.fmt(f),
            Context(ref e) => e.fmt(f),            
        }
    }
}

impl <T> Into<Result<T,ErrorKind>> for ErrorKind {
    fn into(self: ErrorKind) -> Result<T,ErrorKind> {
        Err(self)
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
    WrongUnitOfArguments{index: usize, expected: Vec<String>, got: String},
    PropertyDoesNotExist(String),
    UnitMismatch
}
impl fmt::Display for ApplicationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ApplicationError::*;

        match *self {
            CantFindVariable(ref name, ty) => {
                match ty {
                    VarType::User => write!(f, "The variable '{}' has not been declared", name),
                    VarType::Builtin => write!(f, "The built-in variable '{}' does not exist", name)
                }
            },
            NotAFunction => {
                write!(f, "Trying to use something here as a function, but it is not")
            },
            WrongNumberOfArguments{expected,got} => {
                write!(f, "This function expected {} arguments but got {}", expected, got)
            },
            WrongKindOfArguments{index, ref expected, got} => {
                let e = expected.into_iter().map(|k| k.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "Argument {} is a {}, but the function expected one of {}", index+1, got, e)
            },
            WrongUnitOfArguments{index, ref expected, ref got} => {
                let e = expected.join(", ");
                write!(f, "Argument {} is a unit with type '{}', but the function expected one of {}", index+1, got, e)                
            },
            PropertyDoesNotExist(ref prop) => {
                write!(f, "trying to access the property '{}', which does not exist", prop)
            },
            UnitMismatch => {
                write!(f, "the suffixes of the units need to match but they do not")
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
    Import(Box<Error>, PathBuf),
    CycleDetected(Vec<String>, String)
}
impl fmt::Display for ImportError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ImportError::*;

        match *self {
            CannotImportNoPathSet => {
                write!(f, "I am working from standard in, and so 'import' cannot be used as I don't know where to look for things")
            },
            CannotOpenFile(ref path) => {
                write!(f, "Trying to import the file at {}, but it cannot be opened (perhaps the name is misspelt?)", path.display())
            },
            CannotReadFile(ref path) => {
                write!(f, "Cannot read the file at {} (perhaps you do not have permission?)", path.display())
            },
            ImportLoop(ref paths, ref path) => {
                let cycle = paths
                    .into_iter()
                    .skip_while(|p| p != &path)
                    .chain(iter::once(path))
                    .map(|p| p.display().to_string())
                    .collect::<Vec<_>>()
                    .join("\n  ");
                write!(f, "An import loop was detected:\n  {}", cycle)
            },
            Import(ref err, ref path) => {
                write!(f, "Error while importing {}:\n  {}", path.display(), err)
            },
            CycleDetected(ref vars, ref var) => {
                let cycle = vars
                    .into_iter()
                    .skip_while(|v| v != &var)
                    .chain(iter::once(var))
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(" => ");
                write!(f, "Variables were caught accessing eachother in a cycle:\n  {}", cycle)
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
    NotACSSBlock(Kind)
}
impl fmt::Display for ShapeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ShapeError::*;

        match *self {
            KeyframesKeyvalsNotAllowedAtTop => {
                write!(f, "key:value pairs aren't allowed directly inside a @keyframes block")
            },
            KeyframesKeyframesBlockNotAllowed => {
                write!(f, "@keyframes blocks aren't allowed inside other @keyframes blocks")
            },
            KeyframesFontFaceBlockNotAllowed => {
                write!(f, "@font-face blocks aren't allowed inside @keyframes blocks")
            },
            KeyframesMediaBlockNotAllowed => {
                write!(f, "@media blocks aren't allowed inside @keyframes blocks")
            },
            KeyframesNestedBlockNotAllowed => {
                write!(f, "nested blocks aren't allowed inside @keyframes block sections")
            },
            FontfaceBlockNotAllowed => {
                write!(f, "nested blocks aren't allowed inside @font-face blocks")
            },
            InvalidExpressionInCssValue(ref expr) => {
                write!(f, "I can't handle an expression of type {} inside a CSS value", expr.kind())
            },
            NotACSSBlock(kind) => {
                write!(f, "I expected a CSS block but got a {}", kind)
            }
        }
    }
}

// Errors parsing the syntax into an AST.
#[derive(Clone,PartialEq,Debug)]
pub enum SyntaxError {
    ParseError(String)
}
impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SyntaxError::*;

        match *self {
            ParseError(ref s) => {
                write!(f, "There was an issue parsing:\n{}", s)
            }
        }
    }
}