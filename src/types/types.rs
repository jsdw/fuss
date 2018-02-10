use std::collections::HashMap;
use cache::Cache;
use std::path::{PathBuf,Path};
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
    pub at: At,
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
            Kind::Str => "a string",
            Kind::Bool => "a boolean",
            Kind::Unit => "a unit",
            Kind::Colour => "a colour",
            Kind::Block => "a block",
            Kind::Func => "a function",
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
    Property{ name: String, location: Location },
    Function{ args: Vec<Expression>, location: Location }
}

/// An Expr paired with the start and end position
/// of the underlying text:
#[derive(Debug,Clone)]
pub struct Expression( Rc<ExpressionInner> );
#[derive(Debug,Clone)]
pub struct ExpressionInner{
    pub start: usize,
    pub end: usize,
    pub expr: Expr
}

impl Deref for Expression {
    type Target = ExpressionInner;
    fn deref(&self) -> &Self::Target { &*self.0 }
}

// position independent equality
impl PartialEq for Expression {
    fn eq(&self, other:&Self) -> bool {
        self.expr.eq(&other.expr)
    }
}

impl Expression {
    pub fn with_position(start: usize, end: usize, expr: Expr) -> Expression{
        Expression(Rc::new(ExpressionInner{ start, end, expr }))
    }
    pub fn into_expr(self) -> Option<Expr> {
        match Rc::try_unwrap(self.0) {
            Ok(e) => Some(e.expr),
            Err(_) => None
        }
    }
}

// A version similar to the above but for evaluated expressions.
// During evaluation, we append file info to each expression.
// Otherwise, it's basically the same.
#[derive(Debug,Clone)]
pub struct EvaluatedExpression( Rc<EvaluatedExpressionInner> );
#[derive(Debug,Clone)]
pub struct EvaluatedExpressionInner{
    pub start: usize,
    pub end: usize,
    pub path: Rc<PathBuf>,
    pub expr: EvaluatedExpr
}

impl Deref for EvaluatedExpression {
    type Target = EvaluatedExpressionInner;
    fn deref(&self) -> &Self::Target { &*self.0 }
}

impl PartialEq for EvaluatedExpression {
    fn eq(&self, other:&Self) -> bool {
        self.expr.eq(&other.expr)
    }
}

impl EvaluatedExpression {
    pub fn with_position(start: usize, end: usize, path: Rc<PathBuf>, expr: EvaluatedExpr) -> EvaluatedExpression {
        EvaluatedExpression(Rc::new(EvaluatedExpressionInner{ start, end, path, expr }))
    }
    pub fn new(expr: EvaluatedExpr) -> EvaluatedExpression {
        EvaluatedExpression::with_position(0,0,Rc::new(PathBuf::new()), expr)
    }
    pub fn into_expr(self) -> Option<EvaluatedExpr> {
        match Rc::try_unwrap(self.0) {
            Ok(e) => Some(e.expr),
            Err(_) => None
        }
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
    pub path: Rc<PathBuf>,
    pub root: Rc<PathBuf>,
    pub file_cache: Cache<Rc<PathBuf>,EvaluatedExpr>,
    pub last: Vec<Rc<PathBuf>>
}

impl Context {
    pub fn path_ref(&self) -> &Path {
        &**self.path
    }
    pub fn owned_path(&self) -> PathBuf {
        (*self.path).clone()
    }
    pub fn owned_root(&self) -> PathBuf {
        (*self.root).clone()
    }
    pub fn owned_last(&self) -> Vec<PathBuf> {
        self.last.iter().map(|p| (**p).clone()).collect()
    }
}

// Location of something
#[derive(Clone,PartialEq,Debug)]
pub struct Location {
    start_loc: usize,
    end_loc: usize
}

impl Location {
    pub fn at(start: usize, end: usize) -> Location {
        Location {
            start_loc: start,
            end_loc: end
        }
    }
    pub fn start(&self) -> usize {
        self.start_loc
    }
    pub fn end(&self) -> usize {
        self.end_loc
    }
}