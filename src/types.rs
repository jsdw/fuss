use std::collections::HashMap;

/// Primitive values:
#[derive(PartialEq,Debug,Clone)]
pub enum Primitive {
    Str(String),
    Bool(bool),
    Unit(f64, String)
}

/// The possible things that can crop up in a CSS block
#[derive(PartialEq,Debug,Clone)]
pub enum CSSEntry {
    Expr(Expr),
    KeyVal{ key: String, val: String }
}

/// represents a CSS block, along with any scope that encloses it (variable definitions).
/// one can access their way through scopes using '.', or plonk blocks (and other valid Exprs)
/// into other block's css.
#[derive(PartialEq,Debug,Clone)]
pub struct Block {
    scope: HashMap<String,Expr>,
    selector: String,
    css: Vec<CSSEntry>
}

/// Anything that's an Expression
#[derive(PartialEq,Debug,Clone)]
pub enum Expr {
    /// A primitive eg "hello", 12, 100%, 8px, true, false
    Prim(Primitive),
    /// An if expression eg if this then 2px else 20%
    If{ cond: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr> },
    /// A function eg ($a, $b) => $a + $b
    Func{ inputs: Vec<String>, output: Box<Expr> },
    /// A variable name
    Var(String),
    /// Applying args to something (calling a function)
    App{ expr: Box<Expr>, args: Vec<Expr> },
    /// A CSS block eg { color: red }, or .some.selector { color: blue }
    Block(Block)
}
