use std::collections::HashMap;
use cache::Cache;
use std::path::PathBuf;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use types::colour;
use types::scope::{Scope};
use errors::*;

/// Unevaluated blocks and their parts
#[derive(PartialEq,Debug,Clone)]
pub struct Block {
    pub scope: HashMap<String,Expression>,
    pub selector: Vec<CSSBit>,
    pub css: Vec<CSSEntry>
}

#[derive(PartialEq,Debug,Clone)]
pub enum CSSEntry {
    Expr(Expression),
    KeyVal{ key: Vec<CSSBit>, val: Vec<CSSBit> }
}

#[derive(PartialEq,Debug,Clone)]
pub enum CSSBit {
    Str(String),
    Expr(Expression)
}

/// Evaluated blocks and their parts
#[derive(PartialEq,Debug,Clone)]
pub struct EvaluatedBlock {
    pub ty: BlockType,
    pub start: usize,
    pub end: usize,
    pub scope: HashMap<String,EvaluatedExpression>,
    pub selector: String,
    pub css: Vec<EvaluatedCSSEntry>
}

#[derive(PartialEq,Debug,Clone)]
pub enum EvaluatedCSSEntry {
    Block(EvaluatedBlock),
    KeyVal{ key: String, val: String}
}

#[derive(PartialEq,Debug,Clone,Copy)]
pub enum BlockType {
    Media,
    Keyframes,
    FontFace,
    Generic
}

#[derive(PartialEq,Debug,Clone,Copy)]
pub enum VarType {
    User,
    Builtin
}

/// When describing types, we can use these
#[derive(PartialEq,Debug,Clone,Copy)]
pub enum Kind {
    Str,
    Bool,
    Unit,
    Colour,
    Block,
    Func,
    Undefined
}
impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Kind::Str => "string",
            Kind::Bool => "boolean",
            Kind::Unit => "unit",
            Kind::Colour => "colour",
            Kind::Block => "block",
            Kind::Func => "function",
            Kind::Undefined => "undefined"
        })
    }
}

/// Anything that's an Expression
#[derive(PartialEq,Debug,Clone)]
pub enum Expr {
    /// String eg "hello"
    Str(String),
    /// boolean eg true or false
    Bool(bool),
    /// unit eg 12px, 100%, 30
    Unit(f64, String),
    /// Colour eg #ff0033
    Colour(colour::Colour),
    /// undefined
    Undefined,
    /// A function eg ($a, $b) => $a + $b
    Func{ inputs: Vec<String>, output: Expression },
    /// An if expression eg if this then 2px else 20%
    If{ cond: Expression, then: Expression, otherwise: Expression },
    /// A variable name eg $hello
    Var(String, VarType),
    /// An expression that's being accessed
    Accessed{ expression: Expression, access: Vec<Accessor> },
    /// A CSS block eg { color: red }, or .some.selector { color: blue }
    Block(Block)
}

/// Once an expression has been evaluated, it becomes one of these.
#[derive(PartialEq,Debug,Clone)]
pub enum EvaluatedExpr {
    /// Primitives remain as they are
    Str(String),
    Bool(bool),
    Unit(f64, String),
    Colour(colour::Colour),
    Undefined,
    /// A primitive function; these only show up in Scope and so are always pre-evaluated
    PrimFunc(PrimFunc),
    /// A function eg ($a, $b) => $a + $b. This contains an unevaluated expression that needs evaluating
    /// but gains a Scope over unevaluated functions so we know how to evaluate it.
    Func{ inputs: Vec<String>, output: Expression, scope: Scope },
    /// Evaluated blocks contain pre-evaluated content, ready to display.
    Block(EvaluatedBlock)
}

impl EvaluatedExpr {
    pub fn kind(&self) -> Kind {
        use self::EvaluatedExpr::*;
        match *self {
            Str{..} => Kind::Str,
            Bool{..} => Kind::Bool,
            Unit{..} => Kind::Unit,
            Colour{..} => Kind::Colour,
            Undefined => Kind::Undefined,
            PrimFunc{..} | Func{..} => Kind::Func,
            Block{..} => Kind::Block
        }
    }
}

/// Describes how to dig into a thing to get something out.
#[derive(PartialEq,Debug,Clone)]
pub enum Accessor {
    Property{ name: String },
    Function{ args: Vec<Expression> }
}

/// An Expr paired with the start and end position
/// of the underlying text:
#[derive(Debug,Clone)]
pub struct ExpressionOuter<E>( Rc<ExpressionInner<E>> );
#[derive(Debug,Clone)]
pub struct ExpressionInner<E> {
    pub start: usize,
    pub end: usize,
    pub expr: E
}

/// Aliases for evaluated and unevaluated forms of expr container:
pub type Expression = ExpressionOuter<Expr>;
pub type EvaluatedExpression = ExpressionOuter<EvaluatedExpr>;

impl <E> Deref for ExpressionOuter<E> {
    type Target = ExpressionInner<E>;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl <E> ExpressionOuter<E> {
    pub fn with_position(start: usize, end: usize, expr: E) -> ExpressionOuter<E> {
        ExpressionOuter(Rc::new(ExpressionInner{
            start: start,
            end: end,
            expr: expr
        }))
    }
    pub fn new(expr: E) -> ExpressionOuter<E> {
        ExpressionOuter::with_position(0, 0, expr)
    }
    pub fn into_expr(self) -> Option<E> {
        match Rc::try_unwrap(self.0) {
            Ok(e) => Some(e.expr),
            Err(_) => None
        }
    }
}

// this is so that we can compare expressions, ignoring
// their positions.
impl <E: PartialEq> PartialEq for ExpressionOuter<E> {
    fn eq(&self, other:&Self) -> bool {
        self.expr.eq(&other.expr)
    }
}

/// Wrap our primfuncs into a struct so that we can implement basic
/// traits on them, since they can't be derived automatically.
pub struct PrimFunc(pub fn(&Vec<EvaluatedExpression>,&Context) -> PrimRes);
pub type PrimRes = Result<EvaluatedExpr,ErrorKind>;

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

/// The context in which a thing is evaluated. This is read only and is passed
/// to al prim funcs etc.
pub struct Context {
    pub path: PathBuf,
    pub root: PathBuf,
    pub file_cache: Cache<PathBuf,EvaluatedExpr>,
    pub last: Vec<PathBuf>
}