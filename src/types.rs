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

/// Make use of a variable on scope; either a plain variable call like $hello,
/// or using function syntax the equivalent $hello(), or calling functions eg
/// $hello(2, 3). Chains of these can be created using '.' to scope them where applicable.
/// Infix calls can be represented like PLUS(MINUS(4, 2), MULT(3, 4)) == (4-2) + (3*4)
#[derive(PartialEq,Debug)]
pub struct Application {
    name: String,
    args: Vec<Expr>
}

/// represents a CSS block, along with any scope that encloses it (variable definitions).
/// one can access their way through scopes using '.', or plonk blocks (and other valid Exprs)
/// into other block's css.
#[derive(PartialEq,Debug)]
pub struct Block {
    scope: HashMap<String,Expr>,
    selector: String,
    css: Vec<CSSEntry>
}

/// Anything that's an Expression
#[derive(PartialEq,Debug)]
pub enum Expr {
    /// A primitive eg "hello", 12, 100%, 8px, true, false
    Prim(Primitive),
    /// An if expression eg if this then 2px else 20%
    If{ cond: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr> },
    /// A function eg ($a, $b) => $a + $b
    Func{ inputs: Vec<String>, output: Box<Expr> },
    /// Accessing scopes, eg $hello, $hello.another.thing, $hello(2, 3).$another(true).more.thing
    Accessor(Vec<Application>),
    /// A CSS block eg { color: red }, or .some.selector { color: blue }
    Block(Block)
}
