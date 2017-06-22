use std::collections::HashMap;
use chomp::types::numbering::Numbering;
use chomp::types::Buffer;

//re-export this:
pub use chomp::types::numbering::InputPosition;

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
    Expr(Expression),
    KeyVal{ key: String, val: String }
}

/// represents a CSS block, along with any scope that encloses it (variable definitions).
/// one can access their way through scopes using '.', or plonk blocks (and other valid Exprs)
/// into other block's css.
#[derive(PartialEq,Debug,Clone)]
pub struct Block {
    pub scope: HashMap<String,Expression>,
    pub selector: String,
    pub css: Vec<CSSEntry>
}

/// a simplified version of the above. Nothing is parsed into this type,
/// but it is used as a simplification of Block to prevent some simplification
/// errors.
#[derive(PartialEq,Debug,Clone)]
pub struct NestedSimpleBlock {
    pub selector: String,
    pub css: Vec<NestedCSSEntry>
}

#[derive(PartialEq,Debug,Clone)]
pub enum NestedCSSEntry {
    KeyVal{ key: String, val: String},
    Block(Box<NestedSimpleBlock>)
}


/// Anything that's an Expression
#[derive(PartialEq,Debug,Clone)]
pub enum Expr {
    /// A primitive eg "hello", 12, 100%, 8px, true, false
    Prim(Primitive),
    /// An if expression eg if this then 2px else 20%
    If{ cond: Box<Expression>, then: Box<Expression>, otherwise: Box<Expression> },
    /// A function eg ($a, $b) => $a + $b
    Func{ inputs: Vec<String>, output: Box<Expression> },
    /// A variable name
    Var(String),
    /// Applying args to something (calling a function)
    App{ expr: Box<Expression>, args: Vec<Expression> },
    /// A CSS block eg { color: red }, or .some.selector { color: blue }
    Block(Block),
    /// A simplified version of CSS: we don't have to parse direct to this,
    /// instead we can simplify Block into it later on.
    NestedSimpleBlock(NestedSimpleBlock)
}

/// An Expr paired with the start and end position
/// of the underlying text:
#[derive(PartialEq,Debug,Clone)]
pub struct Expression {
    pub start: Position,
    pub end: Position,
    pub expr: Expr
}

/// a line and column number denoting a position in some text:
#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct Position {
    pub line: u64,
    pub col: u64
}

impl Position {
    pub fn new() -> Position {
        Position{line: 0, col: 0}
    }
}
impl Default for Position {
    fn default() -> Self {
        Position::new()
    }
}

/// by implementing chomp's numbering trait for our Position struct,
/// we can use it inside an InputPosition struct to magically endow our
/// inputs with valid positional information.
impl Numbering for Position {
    type Token = char;

    fn update<B>(&mut self, b: &B) where B: Buffer<Token=Self::Token> {
        let mut col = 0;
        let mut line = 0;

        b.iterate(|c| {
            if c == '\n' { line += 1; col = 0; }
            else { col += 1 }
        });

        self.line += line;
        if line != 0 { self.col = col; }
        else { self.col += col; }
    }

    fn add(&mut self, t: Self::Token) {
        if t == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }
}