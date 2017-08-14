use std::collections::HashMap;
use list::List;
use std::path::PathBuf;
use std::fmt;

/// Primitive values:
#[derive(PartialEq,Debug,Clone)]
pub enum Primitive {
    Str(String),
    Bool(bool),
    Unit(f64, String),
    // This comes up when simplifying if we try to
    // simply an expression defined in terms of itself somehow:
    RecursiveValue
}

/// The possible things that can crop up in a CSS block
#[derive(PartialEq,Debug,Clone)]
pub enum CSSEntry {
    Expr(Expression),
    KeyVal{ key: Vec<CSSBit>, val: Vec<CSSBit> }
}

#[derive(PartialEq,Debug,Clone)]
pub enum EvaluatedCSSEntry {
    Block(EvaluatedBlock),
    KeyVal{ key: String, val: String}
}

/// we parse CSS values into pieces, which are either raw strings
/// or expressions that we'd like interpolated into the output. We
/// allow a limited set of expressions in css values:
///
/// $var
/// $var.a.b
/// ${ arbitrary_expr_here }
#[derive(PartialEq,Debug,Clone)]
pub enum CSSBit {
    Str(String),
    Expr(Expression)
}

/// The different kind of CSS blocks that we know about
#[derive(PartialEq,Debug,Clone)]
pub enum Block<Selector,Inner> {
    KeyframesBlock(KeyframesBlock<Selector,Inner>),
    MediaBlock(MediaBlock<Selector,Inner>),
    FontFaceBlock(FontFaceBlock<Inner>),
    CSSBlock(CSSBlock<Selector,Inner>)
}
pub type UnevaluatedBlock = Block<Vec<CSSBit>, CSSEntry>;

#[derive(PartialEq,Debug,Clone)]
pub struct EvaluatedBlock {
    pub start: Position,
    pub end: Position,
    pub block: Block<String, EvaluatedCSSEntry>
}

/// A CSS block, along with any scope that encloses it (variable definitions).
#[derive(PartialEq,Debug,Clone)]
pub struct CSSBlock<Selector,CSS> {
    pub scope: HashMap<String,Expression>,
    pub selector: Selector,
    pub css: Vec<CSS>
}

/// A keyframes animation block
#[derive(PartialEq,Debug,Clone)]
pub struct KeyframesBlock<Name, Inner> {
    pub scope: HashMap<String,Expression>,
    pub name: Name,
    pub inner: Vec<Inner>
}

/// A media query block.
#[derive(PartialEq,Debug,Clone)]
pub struct MediaBlock<Query,CSS> {
    pub scope: HashMap<String,Expression>,
    pub query: Query,
    pub css: Vec<CSS>
}

/// A font face block.
#[derive(PartialEq,Debug,Clone)]
pub struct FontFaceBlock<CSS> {
    pub scope: HashMap<String,Expression>,
    pub css: Vec<CSS>
}

impl<T,U> Block<T,U> {
    pub fn scope(&self) -> Option<&HashMap<String,Expression>> {
        match *self {
            Block::KeyframesBlock(ref b) => Some(&b.scope),
            Block::MediaBlock(ref b) => Some(&b.scope),
            Block::FontFaceBlock(ref b) => Some(&b.scope),
            Block::CSSBlock(ref b) => Some(&b.scope)
        }
    }
}

/// Anything that's an Expression
#[derive(PartialEq,Debug,Clone)]
pub enum Expr {
    /// A primitive eg "hello", 12, 100%, 8px, true, false
    Prim(Primitive),
    /// A primitive function
    PrimFunc(PrimFunc),
    /// An if expression eg if this then 2px else 20%
    If{ cond: Box<Expression>, then: Box<Expression>, otherwise: Box<Expression> },
    /// A function eg ($a, $b) => $a + $b
    Func{ inputs: Vec<String>, output: Box<Expression>, scope: Scope },
    /// A variable name or accessed variable eg $hello or $hello.there.thing
    Var(String, Vec<String>),
    /// Applying args to something (calling a function)
    App{ expr: Box<Expression>, args: Vec<Expression> },
    /// A CSS block eg { color: red }, or .some.selector { color: blue }
    Block(UnevaluatedBlock),
    /// An evaluated form of the above; this avoids needing to do some checks later:
    EvaluatedBlock(EvaluatedBlock)
}

/// An Expr paired with the start and end position
/// of the underlying text:
#[derive(Debug,Clone)]
pub struct Expression {
    pub start: Position,
    pub end: Position,
    pub expr: Expr
}

// this is so that we can compare expressions, ignoring
// their positions.
impl PartialEq for Expression {
    fn eq(&self, other:&Self) -> bool {
        self.expr.eq(&other.expr)
    }
}

/// Wrap our primfuncs into a struct so that we can implement basic
/// traits on them, since they can't be derived automatically.
pub struct PrimFunc(pub fn(Vec<Expression>,&Context) -> PrimRes);
impl Clone for PrimFunc {
    fn clone(&self) -> PrimFunc {
        PrimFunc(self.0)
    }
}
impl fmt::Debug for PrimFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<<function>>")
    }
}
impl PartialEq for PrimFunc {
    fn eq(&self, other: &PrimFunc) -> bool {
        self.0 as *const usize == other.0 as *const usize
    }
}

/// A stack that we can push values onto in order to represent the
/// current scope at any point in time.
#[derive(Clone,Debug,PartialEq)]
pub struct Scope(List<HashMap<String,Expression>>);
impl Scope {

    /// create a new, empty scope:
    pub fn new() -> Self {
        Scope(List::new().push(HashMap::new()))
    }
    pub fn from(map: HashMap<String,Expression>) -> Self {
        Scope(List::new().push(map))
    }

    /// lookup a value in the scope:
    pub fn find<'a>(&'a self, name: &str) -> Option<&'a Expression> {
        for map in self.0.iter() {
            if let Some(expr) = map.get(name) {
                return Some(expr)
            }
        }
        None
    }

    /// push some new values onto the scope, returning a new one and
    /// leaving the original unchanged:
    pub fn push(&self, values: HashMap<String,Expression>) -> Scope {
        Scope(self.0.push(values))
    }

    /// push one key and value onto the scope, returning a new one and
    /// leaving the original unchanged:
    pub fn push_one(&self, key: String, value: Expression) -> Scope {
        let mut map = HashMap::with_capacity(1);
        map.insert(key, value);
        self.push(map)
    }
}

/// The context in which a thing is evaluated. This is read only and is passed
/// to al prim funcs etc.
#[derive(Clone,Debug,PartialEq)]
pub struct Context {
    pub path: PathBuf,
    pub root: PathBuf
}

#[derive(Copy,Clone,Debug,PartialEq)]
pub struct Position(pub usize);

impl Position {
    pub fn new() -> Position {
        Position(0)
    }
}

pub type Res = Result<Expression,Error>;
pub type PrimRes = Result<Expr,ErrorType>;

/// Error types
#[derive(PartialEq,Debug)]
pub struct Error {
    pub ty: ErrorType,
    pub file: PathBuf,
    pub start: Position,
    pub end: Position
}

#[derive(PartialEq,Debug)]
pub enum ErrorType {
    NotAValidInnerBlock,
    CantFindVariable(String),
    NotAFunction,
    WrongNumberOfArguments{expected: usize, got: usize},
    WrongTypeOfArguments{message: String},
    NotACSSBlock,
    LoopDetected,
    PropertyDoesNotExist(String,String),
    InvalidExpressionInCssValue,
    UnitMismatch,
    CannotOpenFile(PathBuf),
    CannotReadFile(PathBuf),
    ImportError(Box<Error>),
    BlockNotAllowedInFontFace,
    BlockNotAllowedInKeyframes
}