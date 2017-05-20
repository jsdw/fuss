use std::collections::HashMap;

/// Primitive values:
#[derive(PartialEq,Debug)]
pub enum Primitive {
    Str(String),
    Bool(bool),
    Unit(f64, String)
}

/// The possible things that can crop up in a CSS block
#[derive(PartialEq,Debug)]
pub enum CSSEntry {
    Expr(Expr),
    KeyVal{ key: String, val: String }
}
pub type CSSEntryBox = Box<CSSEntry>;

/// Anything that's an Expression
#[derive(PartialEq,Debug)]
pub enum Expr {
    /// A primitive eg "hello", 12, 100%, 8px, true, false
    Prim(Primitive),
    /// A struct eg { $hello: 2, $another: true }
    Struct(HashMap<String,Expr>),
    /// An if expression eg if this then 2px else 20%
    If{ cond: ExprBox, then: ExprBox, otherwise: ExprBox },
    /// A list eg [true, 2px, false]
    List(Vec<Expr>),
    /// A function eg (a, b) => a + b
    Func{ inputs: Vec<String>, output: ExprBox },
    /// A function application eg EXPR + EXPR (infix) or VAR(arg,arg) (normal)
    AppliedFunc{ name: Vec<String>, args: Vec<Expr> },
    /// A variable eg $something or $lark.thing.here
    Var(Vec<String>),
    /// A CSS block eg { color: red }, or .some.selector { color: blue }
    CSS{ selector: String, values: Vec<CSSEntry> },
    /// A scoped expression eg { hi: 2 } => { variable: hi + 4 }
    /// The file will begin with base things in scope like basic operators,
    /// import function, etc.
    Scoped{ scope: HashMap<String,Expr>, inner: ExprBox },
}
pub type ExprBox = Box<Expr>;