use types::*;
use pest::*;
use pest::iterators::*;
use pest::inputs::*;
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use std::collections::{LinkedList,HashMap};

#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("grammar.pest");

#[derive(Parser)]
#[grammar = "./parser/grammar.pest"]
struct MyGrammar;

type MyPair<'a> = Pair<Rule,StrInput<'a>>;
type MySpan<'a> = Span<StrInput<'a>>;

pub fn parse(input: &str) -> Result<Expression,ErrorType> {

    match MyGrammar::parse_str(Rule::file, input) {
        Ok(mut pairs) => {
            // we expect one top level "file" rule at this point, otherwise
            // something has gone seriously wrong. begin parsing from that.
            Ok(file_expression(pairs.next().expect("File expression expected")))
        },
        Err(e) => {
            Err(ErrorType::ParseError(format!("{}", e)))
        }
    }

}

fn file_expression(pair: MyPair) -> Expression {
    to_expression(
        pair.clone().into_span(),
        Expr::Block(css_block_inner_block(pair))
    )
}

fn expression(pair: MyPair) -> Expression {

    // define our precedences:
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::infix0_op, Assoc::Left),
        Operator::new(Rule::infix1_op, Assoc::Left),
        Operator::new(Rule::infix2_op, Assoc::Left),
        Operator::new(Rule::infix3_op, Assoc::Left),
        Operator::new(Rule::infix4_op, Assoc::Left),
        Operator::new(Rule::infix5_op, Assoc::Right)
    ]);

    // eval infix ops:
    let infix = |left: Expression, op: MyPair, right: Expression| -> Expression {
        Expression::with_position(
            left.start,
            right.end,
            Expr::Accessed{
                expression: naked_variable_expression(op),
                access: vec![ Accessor::Function{args:vec![left,right]} ]
            }
        )
    };

    // climb:
    climber.climb(pair.into_inner(), primary_expression, infix)
}

fn naked_variable_expression(pair: MyPair) -> Expression {
    let span = pair.clone().into_span();
    let tok = pair.as_str().to_owned();
    Expression::with_position(
        Position(span.start()),
        Position(span.end()),
        Expr::Var(tok, VarType::Builtin)
    )
}

fn primary_expression(pair: MyPair) -> Expression {
    /*
        // recursing rules:
        (_:expression, expression:_expression()) =>
            expression,
        (_:paren_expression, expression: _expression()) =>
            expression,
        (_:variable_accessor, expression: _expression()) =>
            expression,
        // infix precedence parsing:
        (rule:infix0, expr:_infix()) =>
            expression(rule, expr),
        (rule:infix1, expr:_infix()) =>
            expression(rule, expr),
        (rule:infix2, expr:_infix()) =>
            expression(rule, expr),
        (rule:infix3, expr:_infix()) =>
            expression(rule, expr),
        (rule:infix4, expr:_infix()) =>
            expression(rule, expr),
        (rule:infix5, expr:_infix()) =>
            expression(rule, expr),
        // exprs:
        (rule:function, expr:_function()) =>
            expression(rule, expr),
        (rule:if_then_else, expr:_if_then_else()) =>
            expression(rule, expr),
        (rule:prefix_application, expr:_prefix_application()) =>
            expression(rule, expr),
        (rule:accessible, expr:_accessible()) =>
            expression(rule, expr),
        (rule:accessible_css, expr:_accessible()) =>
            expression(rule, expr),
        (rule:block, expr:_block()) =>
            expression(rule, expr),
        (rule:variable, &var:variable_name) =>
            expression(rule, Expr::Var(var.to_owned(), VarType::User)),
        (rule:naked_variable, &var:variable_name) =>
            expression(rule, Expr::Var(var.to_owned(), VarType::Builtin)),
        // primitives:
        (rule:string, &s:string_contents) =>
            expression(rule, Expr::Str(escaped_string(s))),
        (rule:unit, &n:number, &s:number_suffix) =>
            expression(rule, Expr::Unit( n.parse::<f64>().unwrap(), s.to_owned() )),
        (rule:boolean, _:boolean_true) =>
            expression(rule, Expr::Bool(true)),
        (rule:boolean, _:boolean_false) =>
            expression(rule, Expr::Bool(false)),
        (rule:colour, colour:_colour()) =>
            expression(rule, colour),
        (rule:undefined) =>
            expression(rule, Expr::Undefined),
    */
    unimplemented!()
}

fn function(pair: MyPair) -> Expr {
    /*
        (_:function_args, args:_function_args(), _:function_expression, expr:_expression()) => {
            let arg_vec = args.into_iter().collect::<Vec<String>>();
            Expr::Func{
                inputs: arg_vec,
                output: expr
            }
        },
        (_:function_expression, expr:_expression()) => {
            Expr::Func{
                inputs: vec![],
                output: expr
            }
        },
    */
    unimplemented!()
}

fn function_args(pair: MyPair) -> Vec<String> {
    /*
        (_: variable, &name:variable_name, mut tail: _function_args()) => {
            tail.push_front(name.to_owned());
            tail
        },
        () => {
            LinkedList::new()
        }
    */
    unimplemented!()
}

fn if_then_else(pair: MyPair) -> Expr {
    /*
        (cond: _expression(), then: _expression(), otherwise: _expression()) => {
            Expr::If{
                cond: cond,
                then: then,
                otherwise: otherwise
            }
        }
    */
    unimplemented!()
}

fn colour(pair: MyPair) -> Expr {
    /*
        (&hex:hex_value) => {
            match Colour::from_hex_str(hex) {
                None => Expr::Colour(Colour::transparent()),
                Some(col) => Expr::Colour(col)
            }
        }
    */
    unimplemented!()
}

fn prefix_application(pair: MyPair) -> Expr {
    /*
        (sign:_naked_variable_expression(), _:prefix_application_arg, arg:_expression()) => {
            Expr::Accessed{
                expression: sign,
                access: vec![ Accessor::Function{args:vec![arg]} ]
            }
        },
    */
    unimplemented!()
}

fn accessible(pair: MyPair) -> Expr {
    /*
        (expr: _expression(), accessors: _accessors_start()) => {
            if accessors.len() > 0 {
                Expr::Accessed{
                    expression: expr,
                    access: accessors
                }
            } else {
                expr.into_expr().expect("should be able to unwrap accessible expr")
            }
        }
    */
    unimplemented!()
}

fn accessors_start(pair: MyPair) -> Vec<Accessor> {
    /*
        (_:access, accessors: _accessors()) => {
            accessors.into_iter().collect()
        },
        () => {
            Vec::new()
        }

        // may as well merge this in:

        _accessors(&self) -> LinkedList<Accessor> {
            (_:property_access, &name:variable_name, mut rest: _accessors()) => {
                rest.push_front(Accessor::Property{
                    name: name.to_owned()
                });
                rest
            },
            (_:function_access, _:function_access_args, args:_function_access_args(), mut rest: _accessors()) => {
                rest.push_front(Accessor::Function{
                    args: args.into_iter().collect::<Vec<Expression>>()
                });
                rest
            },
            () => {
                LinkedList::new()
            }
        }

    // and maybe this as well:

        _function_access_args(&self) -> LinkedList<Expression> {
            (_:function_arg, expr:_expression(), mut tail: _function_access_args()) => {
                tail.push_front(expr);
                tail
            },
            () => {
                LinkedList::new()
            }
        }

    */
    unimplemented!()
}

fn css_block_inner_block(pair: MyPair) -> Block {
    let inner = css_block_inner(pair);
    Block {
        scope:inner.scope,
        css:inner.css,
        selector:vec![]
    }
}

fn block(pair: MyPair) -> Expr {
    /*
        (_:block_selector, selector:_block_selector(), _:block_open, inner:_css_block_inner(), _:block_close) => {
            Expr::Block(Block{
                selector: selector.into_iter().collect::<Vec<CSSBit>>(),
                scope: inner.scope,
                css: inner.css
            })
        }
    */
    unimplemented!()
}

fn block_selector(pair: MyPair) -> Vec<CSSBit> {
    /*
        _block_selector(&self) -> LinkedList<CSSBit> {
            (&chars:block_selector_chars, mut tail: _block_selector()) => {
                tail.push_front( CSSBit::Str(chars.to_owned()) );
                tail
            },
            (_:block_interpolated_expression, expr:_expression(), mut tail: _block_selector()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
    */
    unimplemented!()
}

fn css_block_inner(pair: MyPair) -> CSSBlockInner {
    let inner = css_block_inner_pieces(pair);
    let mut scope = HashMap::new();
    let mut css = vec![];
    for val in inner.into_iter() {
        match val {
            CSSBlockInnerPiece::Scope(key,val) => {
                scope.insert(key,val);
            },
            CSSBlockInnerPiece::CSS(entry) => {
                css.push(entry);
            }
        }
    }
    CSSBlockInner{
        scope: scope,
        css: css
    }
}

fn css_block_inner_pieces(pair: MyPair) -> Vec<CSSBlockInnerPiece> {
    let out = vec![];
    for pair in pair.into_inner() {

        match pair.as_rule() {
            Rule::block_assignment => {
                let mut inner = pair.into_inner();
                let varname = variable(inner.next().unwrap());
                let expression = expression(inner.next().unwrap());
                out.push(CSSBlockInnerPiece::Scope(varname.to_owned(),expression));
            },
            Rule::block_expression => {
                let mut inner = pair.into_inner();
                let expression = expression(inner.next().unwrap());
                out.push(CSSBlockInnerPiece::CSS( CSSEntry::Expr( expression ) ));
            },
            Rule::block_css => {
                let mut inner = pair.into_inner();
                let block = block_css(inner.next().unwrap());
                out.push(CSSBlockInnerPiece::CSS( block ));
            }
        }

    }
    out

}

fn block_css(pair: MyPair) -> CSSEntry {
    /*
        _block_css(&self) -> CSSEntry {
            (_:block_css_key, key:_block_css_key(), _:block_css_value, val:_block_css_val()) => {
                CSSEntry::KeyVal{
                    key: key.into_iter().collect::<Vec<CSSBit>>(),
                    val: val.into_iter().collect::<Vec<CSSBit>>()
                }
            }
        }
    */
    unimplemented!()
}

fn block_css_key(pair: MyPair) -> Vec<CSSBit> {
    /*
        _block_css_key(&self) -> LinkedList<CSSBit> {
            (_:block_interpolated_expression, expr:_expression(), mut tail:_block_css_key()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            (&chars:block_css_key_chars, mut tail:_block_css_key()) => {
                tail.push_front( CSSBit::Str(chars.trim().to_owned()) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
    */
    unimplemented!()
}

fn block_css_val(pair: MyPair) -> Vec<CSSBit> {
    /*
        _block_css_val(&self) -> LinkedList<CSSBit> {
            (_:block_interpolated_expression, expr:_expression(), mut tail:_block_css_val()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            (_:variable_accessor, expr: _expression(), mut tail:_block_css_val()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            (&chars:block_css_value_chars, mut tail:_block_css_val()) => {
                tail.push_front( CSSBit::Str(chars.to_owned()) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
    */
    unimplemented!()
}

//
// Helpers:
//

fn to_expression(span: MySpan, expr: Expr) -> Expression {
    Expression::with_position(
        ::types::Position(span.start()),
        ::types::Position(span.end()),
        expr
    )
}

// escape a string - allow \\ and \" in a string; resulting in \ and ".
fn escaped_string(s: &str) -> String {
    let mut out = String::new();
    let mut is_escaped = false;
    for c in s.chars() {
        if !is_escaped && c == '\\' {
           is_escaped = true;
        } else {
            out.push(c);
            is_escaped = false;
        }
    }
    out
}

// a quick struct to let us build up a linkedlist of block inner pieces:
#[derive(Clone,Debug,PartialEq)]
pub enum CSSBlockInnerPiece {
    Scope(String,Expression),
    CSS(CSSEntry)
}

#[derive(Clone,Debug,PartialEq)]
pub struct CSSBlockInner {
    pub scope: HashMap<String,Expression>,
    pub css: Vec<CSSEntry>
}

/*
// parse a string into an expression
pub fn parse(input: &str) -> Result<Expression,ErrorType> {

    let mut parser = Rdp::new(StringInput::new(input));

    if !parser.file() {
        return Err(ErrorType::NotAValidInnerBlock)
    }
    Ok(parser.file_expression())

}

// turn a Token + Expr into an Expression:
fn expression(rule: Token<Rule>, expr: Expr) -> Expression {
    Expression::with_position(
        Position(rule.start),
        Position(rule.end),
        expr
    )
}

// escape a string - allow \\ and \" in a string; resulting in \ and ".
fn escaped_string(s: &str) -> String {
    let mut out = String::new();
    let mut is_escaped = false;
    for c in s.chars() {
        if !is_escaped && c == '\\' {
           is_escaped = true;
        } else {
            out.push(c);
            is_escaped = false;
        }
    }
    out
}

// a quick struct to let us build up a linkedlist of block inner pieces:
#[derive(Clone,Debug,PartialEq)]
pub enum CSSBlockInnerPiece {
    Scope(String,Expression),
    CSS(CSSEntry)
}

#[derive(Clone,Debug,PartialEq)]
pub struct CSSBlockInner {
    pub scope: HashMap<String,Expression>,
    pub css: Vec<CSSEntry>
}

impl_rdp! {
    grammar! {

        // IN FILE NOW

    }

    process!{
        file_expression(&self) -> Expression {
            (rule:file, block:_css_block_inner_block()) => {
                expression(rule, Expr::Block(block))
            }
        }
        _expression(&self) -> Expression {
            // recursing rules:
            (_:expression, expression:_expression()) =>
                expression,
            (_:paren_expression, expression: _expression()) =>
                expression,
            (_:variable_accessor, expression: _expression()) =>
                expression,
            // infix precedence parsing:
            (rule:infix0, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix1, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix2, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix3, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix4, expr:_infix()) =>
                expression(rule, expr),
            (rule:infix5, expr:_infix()) =>
                expression(rule, expr),
            // exprs:
            (rule:function, expr:_function()) =>
                expression(rule, expr),
            (rule:if_then_else, expr:_if_then_else()) =>
                expression(rule, expr),
            (rule:prefix_application, expr:_prefix_application()) =>
                expression(rule, expr),
            (rule:accessible, expr:_accessible()) =>
                expression(rule, expr),
            (rule:accessible_css, expr:_accessible()) =>
                expression(rule, expr),
            (rule:block, expr:_block()) =>
                expression(rule, expr),
            (rule:variable, &var:variable_name) =>
                expression(rule, Expr::Var(var.to_owned(), VarType::User)),
            (rule:naked_variable, &var:variable_name) =>
                expression(rule, Expr::Var(var.to_owned(), VarType::Builtin)),
            // primitives:
            (rule:string, &s:string_contents) =>
                expression(rule, Expr::Str(escaped_string(s))),
            (rule:unit, &n:number, &s:number_suffix) =>
                expression(rule, Expr::Unit( n.parse::<f64>().unwrap(), s.to_owned() )),
            (rule:boolean, _:boolean_true) =>
                expression(rule, Expr::Bool(true)),
            (rule:boolean, _:boolean_false) =>
                expression(rule, Expr::Bool(false)),
            (rule:colour, colour:_colour()) =>
                expression(rule, colour),
            (rule:undefined) =>
                expression(rule, Expr::Undefined),
        }
        _infix(&self) -> Expr {
            (left:_expression(), sign:_naked_variable_expression(), right:_expression()) => {
                Expr::Accessed{
                    expression: sign,
                    access: vec![ Accessor::Function{args:vec![left,right]} ]
                }
            }
        }
        _naked_variable_expression(&self) -> Expression {
            (sign) => {
                let tok = self.input().slice(sign.start,sign.end);
                Expression::with_position(
                    Position(sign.start),
                    Position(sign.end),
                    Expr::Var(tok.to_owned(), VarType::Builtin)
                )
            }
        }
        _function(&self) -> Expr {
            (_:function_args, args:_function_args(), _:function_expression, expr:_expression()) => {
                let arg_vec = args.into_iter().collect::<Vec<String>>();
                Expr::Func{
                    inputs: arg_vec,
                    output: expr
                }
            },
            (_:function_expression, expr:_expression()) => {
                Expr::Func{
                    inputs: vec![],
                    output: expr
                }
            },
        }
        _function_args(&self) -> LinkedList<String> {
            (_: variable, &name:variable_name, mut tail: _function_args()) => {
                tail.push_front(name.to_owned());
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _if_then_else(&self) -> Expr {
            (cond: _expression(), then: _expression(), otherwise: _expression()) => {
                Expr::If{
                    cond: cond,
                    then: then,
                    otherwise: otherwise
                }
            }
        }
        _colour(&self) -> Expr {
            (&hex:hex_value) => {
                match Colour::from_hex_str(hex) {
                    None => Expr::Colour(Colour::transparent()),
                    Some(col) => Expr::Colour(col)
                }
            }
        }
        _prefix_application(&self) -> Expr {
            (sign:_naked_variable_expression(), _:prefix_application_arg, arg:_expression()) => {
                Expr::Accessed{
                    expression: sign,
                    access: vec![ Accessor::Function{args:vec![arg]} ]
                }
            },
        }
        _accessible(&self) -> Expr {
            (expr: _expression(), accessors: _accessors_start()) => {
                if accessors.len() > 0 {
                    Expr::Accessed{
                        expression: expr,
                        access: accessors
                    }
                } else {
                    expr.into_expr().expect("should be able to unwrap accessible expr")
                }
            }
        }
        _accessors_start(&self) -> Vec<Accessor> {
            (_:access, accessors: _accessors()) => {
                accessors.into_iter().collect()
            },
            () => {
                Vec::new()
            }
        }
        _accessors(&self) -> LinkedList<Accessor> {
            (_:property_access, &name:variable_name, mut rest: _accessors()) => {
                rest.push_front(Accessor::Property{
                    name: name.to_owned()
                });
                rest
            },
            (_:function_access, _:function_access_args, args:_function_access_args(), mut rest: _accessors()) => {
                rest.push_front(Accessor::Function{
                    args: args.into_iter().collect::<Vec<Expression>>()
                });
                rest
            },
            () => {
                LinkedList::new()
            }
        }
        _function_access_args(&self) -> LinkedList<Expression> {
            (_:function_arg, expr:_expression(), mut tail: _function_access_args()) => {
                tail.push_front(expr);
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _css_block_inner_block(&self) -> Block {
            (inner:_css_block_inner()) => {
                Block {
                    scope:inner.scope,
                    css:inner.css,
                    selector:vec![]
                }
            }
        }
        _block(&self) -> Expr {
            (_:block_selector, selector:_block_selector(), _:block_open, inner:_css_block_inner(), _:block_close) => {
                Expr::Block(Block{
                    selector: selector.into_iter().collect::<Vec<CSSBit>>(),
                    scope: inner.scope,
                    css: inner.css
                })
            }
        }
        _block_selector(&self) -> LinkedList<CSSBit> {
            (&chars:block_selector_chars, mut tail: _block_selector()) => {
                tail.push_front( CSSBit::Str(chars.to_owned()) );
                tail
            },
            (_:block_interpolated_expression, expr:_expression(), mut tail: _block_selector()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _css_block_inner(&self) -> CSSBlockInner {
            (inner: _css_block_inner_pieces()) => {
                let mut scope = HashMap::new();
                let mut css = vec![];
                for val in inner.into_iter() {
                    match val {
                        CSSBlockInnerPiece::Scope(key,val) => {
                            scope.insert(key,val);
                        },
                        CSSBlockInnerPiece::CSS(entry) => {
                            css.push(entry);
                        }
                    }
                }
                CSSBlockInner{
                    scope: scope,
                    css: css
                }
            }
        }
        _css_block_inner_pieces(&self) -> LinkedList<CSSBlockInnerPiece> {
            (_:block_assignment, _:block_variable_assign, _:variable, &var:variable_name, expr:_expression(), mut tail:_css_block_inner_pieces()) => {
                tail.push_front( CSSBlockInnerPiece::Scope(var.to_owned(),expr) );
                tail
            },
            (_:block_expression, expr:_expression(), mut tail:_css_block_inner_pieces()) => {
                tail.push_front( CSSBlockInnerPiece::CSS( CSSEntry::Expr( expr ) ) );
                tail
            },
            (_:block_css, entry:_block_css(), mut tail:_css_block_inner_pieces()) => {
                tail.push_front( CSSBlockInnerPiece::CSS( entry ) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _block_css(&self) -> CSSEntry {
            (_:block_css_key, key:_block_css_key(), _:block_css_value, val:_block_css_val()) => {
                CSSEntry::KeyVal{
                    key: key.into_iter().collect::<Vec<CSSBit>>(),
                    val: val.into_iter().collect::<Vec<CSSBit>>()
                }
            }
        }
        _block_css_key(&self) -> LinkedList<CSSBit> {
            (_:block_interpolated_expression, expr:_expression(), mut tail:_block_css_key()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            (&chars:block_css_key_chars, mut tail:_block_css_key()) => {
                tail.push_front( CSSBit::Str(chars.trim().to_owned()) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
        _block_css_val(&self) -> LinkedList<CSSBit> {
            (_:block_interpolated_expression, expr:_expression(), mut tail:_block_css_val()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            (_:variable_accessor, expr: _expression(), mut tail:_block_css_val()) => {
                tail.push_front( CSSBit::Expr(expr) );
                tail
            },
            (&chars:block_css_value_chars, mut tail:_block_css_val()) => {
                tail.push_front( CSSBit::Str(chars.to_owned()) );
                tail
            },
            () => {
                LinkedList::new()
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    fn s(s: &str) -> String {
        s.to_owned()
    }

    fn e(e: Expr) -> Expression {
        Expression::with_position(Position::new(), Position::new(), e)
    }

    fn var(name: &str) -> Expression {
        e(Expr::Var(s(name), VarType::User))
    }

    fn builtin(name: &str) -> Expression {
        e(Expr::Var(s(name), VarType::Builtin))
    }

    fn b(b: bool) -> Expression {
        e(Expr::Bool(b))
    }

    fn unit(n: f64, s: &str) -> Expression {
        e(Expr::Unit(n,s.to_owned()))
    }

    fn app(expression: Expression, args: Vec<Accessor>) -> Expression {
        e(Expr::Accessed{
            expression: expression,
            access: args
        })
    }

    fn a_prop(name: &str) -> Accessor {
        Accessor::Property{ name: s(name) }
    }

    fn a_fn(args: Vec<Expression>) -> Accessor {
        Accessor::Function{ args: args }
    }

    fn op(expression: Expression, args: Vec<Expression>) -> Expression {
        e(Expr::Accessed{
            expression: expression,
            access: vec![ a_fn(args) ]
        })
    }

    // make defining hash maps a little easier:
    macro_rules! hash_map {
        ( $($key:expr => $val:expr);+ ) => ({
            use std::collections::HashMap;
            let mut map = HashMap::new();
            $( map.insert($key, $val); )*
            map
        });
        ( ) => ({
            use std::collections::HashMap;
            HashMap::new()
        })
    }

    // test the parsing aspect:
    macro_rules! parse_test {

        // create test function:
        ( $func:ident; $($rest:tt)+ ) => (
            #[test]
            #[allow(unused)]
            fn $func() {
                use std::fmt::Write;
                let mut errors = String::new();
                parse_test!{ __SINGLE (errors) $($rest)+ }
                if errors.len() > 0 {
                    assert!(false, "\n{}", errors);
                }
            }
        );

        ( __SINGLE ($errors:ident) ) => ();

        // str => variable[ child, child.. ]
        ( __SINGLE ($errors:ident) $input:expr => $token:ident[ $($inner:ident $children:tt),* ]; $($rest:tt)* ) => (
            {
                let s = $input.to_owned();
                let mut parser = Rdp::new(StringInput::new($input));

                if !parser.$token(){
                    writeln!(&mut $errors, "ERROR: could not parse!\n - wants: {:?}\n - input: \n{}\n - expected: {:?}\n - got: {:?}", parser.expected(), s, Rule::$token, parser.queue());
                }
                else if !parser.end() {
                    writeln!(&mut $errors, "ERROR: didn't end when expected!\n - wants: {:?}\n - input: \n{}\n - expected: {:?}\n - got: {:?}", parser.expected(), s, Rule::$token, parser.queue());
                }
                else {
                    assert_eq!(s.len(), parser.queue()[0].end);
                }
            }
            parse_test!{ __SINGLE ($errors) $($rest)* }
        );

    }

    // test the processing aspect:
    macro_rules! process_test {

        // create test function:
        ( $func:ident; $($rest:tt)+ ) => (
            #[test]
            #[allow(unused)]
            fn $func() {
                use std::fmt::Write;
                let mut errors = String::new();
                process_test!{ __SINGLE (errors) $($rest)+ }
                if errors.len() > 0 {
                    assert!(false, "\n{}", errors);
                }
            }
        );

        ( __SINGLE ($errors:ident) ) => ();

        // run a test to see whether the string on the left is turned into the expression on the right:
        ( __SINGLE ($errors:ident) $input:expr => $output:expr; $($rest:tt)* ) => (
            {
                let mut test = || {
                    use std::panic;
                    let mut parser = Rdp::new(StringInput::new($input));
                    if !parser.expression() {
                        writeln!(&mut $errors, "\nERROR: processor failed to parse expression!\n - input: {:?}\n - expected: {:?}", $input, $output);
                        return
                    }

                    let res = panic::catch_unwind(panic::AssertUnwindSafe(|| parser._expression()));
                    match res {
                        Ok(expr) => {
                            if expr != $output {
                                writeln!(&mut $errors,
                                    "\nERROR: exprs did not match!\n - input: {:?}\n - expected: {:?}\n - got: {:?}\n - tokens: {:?}",
                                    $input,
                                    $output,
                                    expr,
                                    parser.queue()
                                );
                            }
                        },
                        Err(_) => {
                            writeln!(&mut $errors,
                                "\nERROR: process panic!\n - input: {:?}\n - expected: {:?}\n - tokens: {:?}",
                                $input,
                                $output,
                                parser.queue()
                            );
                        }
                    };
                };
                test();
            }
            process_test!( __SINGLE ($errors) $($rest)*);
        );

        // run a test to see whether the string on the left is turned into the same expression as the string on the right:
        ( __SINGLE ($errors:ident) $a:expr , $b:expr; $($rest:tt)* ) => (
            {
                let mut test = || {
                    use std::panic;

                    let mut parser1 = Rdp::new(StringInput::new($a));
                    if !parser1.expression() {
                        writeln!(&mut $errors, "\nERROR: cmp: processor failed to parse expression!\n - input: {:?}", $a);
                        return
                    }
                    let mut parser2 = Rdp::new(StringInput::new($b));
                    if !parser2.expression() {
                        writeln!(&mut $errors, "\nERROR: cmp: processor failed to parse expression!\n - input: {:?}", $b);
                        return
                    }

                    let res1 = panic::catch_unwind(panic::AssertUnwindSafe(|| parser1._expression()));
                    let res2 = panic::catch_unwind(panic::AssertUnwindSafe(|| parser2._expression()));

                    if res1.is_err() {
                        writeln!(&mut $errors,
                            "\nERROR: cmp: failed to parse left string into an expression!\n - input: {:?}\n - tokens: {:?}",
                            $a,
                            parser1.queue()
                        );
                    }
                    if res2.is_err() {
                        writeln!(&mut $errors,
                            "\nERROR: cmp: failed to parse right string into an expression!\n - input: {:?}\n - tokens: {:?}",
                            $b,
                            parser2.queue()
                        );
                    }

                    let res1e = res1.unwrap();
                    let res2e = res2.unwrap();

                    if res1e != res2e {
                        writeln!(&mut $errors,
                            "\nERROR: cmp: exprs did not match!\n - left: {:?}\n - right: {:?}",
                            res1e,
                            res2e
                        );
                    }
                };
                test();

            }
            process_test!( __SINGLE ($errors) $($rest)*);
        )

    }

    parse_test!{test_variable;
        "$hello" => variable[];
        "$2ello" => variable[];
        "$_hello" => variable[];
        "$hello" => expression[];
        "$2ello" => expression[];
        "$_hello" => expression[];
    }

    process_test!{test_variable_e;
        "$hello" => var("hello");
        "$hello.there" => app(var("hello"), vec![a_prop("there")]);
        "$hello.there.you" => app(var("hello"), vec![a_prop("there"), a_prop("you")]);
    }

    parse_test!{test_numeric;
        "20" => number[];
        "-20" => number[];
        "0.12" => number[];
        "100.0" => number[];
        "100.0px" => unit[];
        "100.0em" => unit[];
        "0%" => unit[];
        "100.0px" => expression[];
        "100.0em" => expression[];
        "0%" => expression[];
    }

    parse_test!{text_colour_hex;
        "#fef" => colour[];
        "#fef0" => colour[];
        "#123456" => colour[];
        "#12345678" => colour[];
        "#09afAF" => colour[];
    }

    process_test!{test_number_e;
        "10" => unit(10.0,"");
        "0" => unit(0.0,"");
        "0.12" => unit(0.12,"");
        "0%" => unit(0.0,"%");
        "100.0px" => unit(100.0,"px");
    }

    parse_test!{test_if_then_else;
        "if true then  true \nelse true" => if_then_else[];
        "if 20px then 10 else 100" => if_then_else[];
        "if true then true else true" => expression[];
        "if 20px then 10 else 100" => expression[];
    }

    parse_test!{test_strings;
        r#""hello there""# => string[];
        r#""hello \"hello\" there""# => string[];
        r#""hello there""# => expression[];
        r#""hello \"hello\" there""# => expression[];
    }

    process_test!{test_strings_e;
        r#""hello""# =>
            e(Expr::Str(s("hello")));
        r#""""# =>
            e(Expr::Str(s("")));
        r#""escaped \"lark\"""# =>
            e(Expr::Str(s(r#"escaped "lark""#)));
        r#"" \\lark\\ ""# =>
            e(Expr::Str(s(r#" \lark\ "#)));
    }

    parse_test!{test_functions;
        "() => true" => function[];
        "($lark, $another) => $lark" => function[];
    }

    process_test!{test_functions_e;
        "($apPl_3s, $b2ananA) => true" =>
            e(Expr::Func{
                inputs: vec![s("apPl_3s"),s("b2ananA")],
                output: b(true)
            });
        "(\n$a\t,\n\t \n$b\n)\n\t\t=>\n\t\tfalse" =>
            e(Expr::Func{
                inputs: vec![s("a"),s("b")],
                output: b(false)
            });
    }

    parse_test!{test_operators;
        "20 + 40" => expression[];
        "20 / 40" => expression[];
        "20 * 40" => expression[];
        "20 - 40" => expression[];
    }

    process_test!{test_precedence_e;
        "1 + 2 + 3"         , "(1 + 2) + 3";
        "1 + 2 * 3 + 4"     , "1 + (2 * 3) + 4";
        "1 + 2 * 3 * 4 + 5" , "1 + (2 * 3 * 4) + 5";
        "1 + 2 * 3 * 4 + 5" , "(1 + ((2 * 3) * 4)) + 5";
        "1 * 2 / 3 * 4"     , "((1 * 2) / 3) * 4";
        "-1+2 + 3"          , "(-1) + 2 + 3";
        "1 + -2 +3"         , "1 + (-2) + 3";
        "3 + 2 ^ 2 ^ 2 ^ 2" , "3 + (2 ^ (2 ^ (2 ^ 2)))";
    }

    parse_test!{test_func_applications;
        "$hello(2px,true)" => expression[];
        "$hello(2px, true)" => expression[];
        "$hello(2px, true)" => accessible[];
        "!$hello" => expression[];
        "!($hello)" => expression[];
        "!$hello()" => expression[];
        "!($hello())" => expression[];
        "!$hello(2px)" => expression[];
        "!($hello(2px))" => expression[];
        "!($hello(2px, true))" => expression[];
        "!$hello(2px, true)" => expression[];
        "!$hello()" => prefix_application[];
        "!$hello(1px)" => prefix_application[];
        "!$hello(1px, true)" => prefix_application[];
        "!($hello)" => prefix_application[];
        "!($hello())" => prefix_application[];
        "!($hello(1px))" => prefix_application[];
        "!($hello(1px, true))" => prefix_application[];
        "$hello.there($a).b" => expression[];
    }

    process_test!{test_func_applications_e;
        "$hello(2px, true)" =>
            op(
                var("hello"),
                vec![ unit(2.0,"px"), b(true) ]
            );
        "!$hello" =>
            op(
                builtin("!"),
                vec![ var("hello") ]
            );
        "!$hello.there" =>
            op(
                builtin("!"),
                vec![ app( var("hello"), vec![ a_prop("there") ]) ]
            );
        "!$hello()" =>
            op(
                builtin("!"),
                vec![ app( var("hello"), vec![a_fn(vec![])] ) ]
            );
        "!$hello().b" =>
            op(
                builtin("!"),
                vec![ app( var("hello"), vec![a_fn(vec![]),a_prop("b")] ) ]
            );
        "!$hello($a)" =>
            op(
                builtin("!"),
                vec![ app( var("hello"), vec![ a_fn(vec![var("a")]) ] ) ]
            );
        "$hello.there($a)" =>
            app(
                var("hello"),
                vec![ a_prop("there"), a_fn(vec![var("a")]) ]
            );
        "builtin_thing($a)" =>
            app(
                builtin("builtin_thing"),
                vec![ a_fn(vec![var("a")]) ]
            );
        "builtin_thing(builtin_var)" =>
            app(
                builtin("builtin_thing"),
                vec![ a_fn(vec![builtin("builtin_var")]) ]
            );
        "$hello.there(1px).b" =>
            app(
                var("hello"),
                vec![ a_prop("there"), a_fn(vec![unit(1.0,"px")]), a_prop("b") ]
            );
        "$hello.there($a).b" =>
            app(
                var("hello"),
                vec![ a_prop("there"), a_fn(vec![var("a")]), a_prop("b") ]
            );
        "$hello.there($a).b.c($d,$e)" =>
            app(
                var("hello"),
                vec![ a_prop("there"), a_fn(vec![var("a")]), a_prop("b"), a_prop("c"), a_fn(vec![var("d"),var("e")]) ]
            );
        "!$hello.there($a)" =>
            op(
                builtin("!"),
                vec![ app(var("hello"), vec![ a_prop("there"), a_fn(vec![var("a")]) ]) ]
            );
        "!$hello.there($a) + 2px" =>
            op(
                builtin("+"),
                vec![
                    op(
                        builtin("!"),
                        vec![ app(var("hello"), vec![ a_prop("there"), a_fn(vec![var("a")]) ]) ]
                    ),
                    unit(2.0, "px")
                ]
            );
    }

    parse_test!{test_block_css;
        "hello: there;" => block_css[];
        "${ $hello }: there;" => block_css[];
        "${ $hello }: the${ 2px }re;" => block_css[];
        "${ $hello }: the${ 2px * 2 }re;" => block_css[];
        "hello: the${ 2px * 2 }re;" => block_css[];
    }

    parse_test!{test_block_assignment;
        "$hello: 2px;" => block_assignment[];
        "$hello: ($a, $b) => $a + $b;" => block_assignment[];
        "$hello: {};" => block_assignment[];
    }

    parse_test!{test_block_with_css_only;
        "{}" => block[];
        ".stuff {}" => block[];
        ".stuff {}" => expression[];
        "{ hello: there; }" => block[];
        "{ ${ $hello }: there; }" => block[];
        "{ ${ $hello }: the${ 2px }re; }" => block[];
        "{ border: $a.b.c }" => expression[];
    }

    process_test!{test_empty_block_e;
        ".some-class {
            /* empty */
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![],
                css: vec![]
            }));
    }

    process_test!{test_block_e;
        ".some-class {
            hi: $a px;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![],
                css: vec![
                    CSSEntry::KeyVal{
                        key: vec![CSSBit::Str(s("hi"))],
                        val: vec![
                            CSSBit::Expr(var("a")),
                            CSSBit::Str(s(" px"))
                        ]
                    },
                ]
            }));
        ".some-class {
            $hello: 2px;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![
                    s("hello") => unit(2f64,"px")
                ],
                css: vec![]
            }));
        ".some-class {
            $hello: ($a, $b) => true;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![
                    s("hello") => e(Expr::Func{
                        inputs: vec![s("a"), s("b")],
                        output: e(Expr::Bool(true))
                    })
                ],
                css: vec![]
            }));
        ".some-class {
            $hello: { };
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![
                    s("hello") => e(Expr::Block(Block{
                        selector: vec![],
                        scope: hash_map![],
                        css: vec![]
                    }))
                ],
                css: vec![]
            }));
        ".some-class {
            $hello;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![],
                css: vec![
                    CSSEntry::Expr(var("hello"))
                ]
            }));
        ".some-class {
            $hello: ($a, $b) => true;
            $hello;
        }" =>
            e(Expr::Block(Block{
                selector: vec![CSSBit::Str(s(".some-class "))],
                scope: hash_map![
                    s("hello") => e(Expr::Func{
                        inputs: vec![s("a"), s("b")],
                        output: b(true)
                    })
                ],
                css: vec![
                    CSSEntry::Expr(var("hello"))
                ]
            }));
        "{
            border: 1px solid black;
            {
                lark: another thing hereee;
            }
        }" =>
            e(Expr::Block(Block{
                scope: hash_map![],
                selector: vec![],
                css: vec![
                    CSSEntry::KeyVal{
                        key: vec![CSSBit::Str(s("border"))],
                        val: vec![CSSBit::Str(s("1px solid black"))]
                    },
                    CSSEntry::Expr(e(Expr::Block(Block{
                        scope: hash_map![],
                        selector: vec![],
                        css: vec![
                            CSSEntry::KeyVal{
                                key: vec![CSSBit::Str(s("lark"))],
                                val: vec![CSSBit::Str(s("another thing hereee"))]
                            }
                        ]
                    })))
                ]
            }));
        "{ border: $o.width; }" =>
            e(Expr::Block(Block{
                scope: hash_map![],
                selector: vec![],
                css: vec![
                    CSSEntry::KeyVal{
                        key: vec![CSSBit::Str(s("border"))],
                        val: vec![CSSBit::Expr(app(
                            var("o"),
                            vec![ a_prop("width") ]
                        ))]
                    },
                ]
            }));
        "{ .hello { a:1; }; .another { b:1; }; }" =>
            e(Expr::Block(Block{
                scope: hash_map![],
                selector: vec![],
                css: vec![
                    CSSEntry::Expr(e(Expr::Block(Block{
                        scope: hash_map![],
                        selector: vec![CSSBit::Str(s(".hello "))],
                        css: vec![
                            CSSEntry::KeyVal{
                                key: vec![CSSBit::Str(s("a"))],
                                val: vec![CSSBit::Str(s("1"))]
                            }
                        ]
                    }))),
                    CSSEntry::Expr(e(Expr::Block(Block{
                        scope: hash_map![],
                        selector: vec![CSSBit::Str(s(".another "))],
                        css: vec![
                            CSSEntry::KeyVal{
                                key: vec![CSSBit::Str(s("b"))],
                                val: vec![CSSBit::Str(s("1"))]
                            }
                        ]
                    })))
                ]
            }));
    }

    parse_test!(test_file_expression;
        "$hello: 1px; $another: 2px;" => file[];
    );

    parse_test!(test_endings;
        "{ $a: 2 }" => block[]; //no semicolon or newline ending assignment OK at end of block
        "{ $a: 2; .hello.there {} }" => block[]; //block ending OK at end of block
        "{{{}};{}}" => block[]; //empty blocks are fine next to eachother.
    );

    parse_test!{test_block_more;
        "{ $hello: 1px; ${ $hello }: the${ 2px }re; }" => block[];
        ".some-selector value here::after { $hello: 1px; ${ $hello }: the${ 2px }re; }" => block[];
        ".some-selector value here::after { $hello: 1px; -hello-${ $world }: the${ 2px }re; }" => block[];
        ".some-class:not(:last-child) {}" => block[];
        ".some-class:not(:last-child) {
            $hello: ($a, $b) => $a + $b;
            $another: 2;
            $lark: { $sub1: 2px; };
            $more: $lark.sub1;
        }" => block[];
        ".some-class:not(:last-child) {
            $hello(2px, 5px);
        }" => block[];
        ".some-class:not(:last-child) {
            $hello;
        }" => block[];
        ".some-class:not(:last-child) {
            $hello: ($a, $b) => $a + $b;
            $another: 2;
            $lark: { $sub1: 2px; };
            $more: $lark.sub1;
            /* block /* nested!! */ comment */

            -moz-background-color: 1px solid blue;
            border-radius: 10px;
        }" => block[];
        ".some-class:not(:last-child) {
            /* a comment till end of line
             * lark
             * woop
             */
            & .a-sub-class .more.another, .sub-clas-two { color: red; $subThing: -2px + 4px; }
            -moz-background-color: 1px solid blue;
            border-radius: 10px;
        }" => block[];
        "{ $lark: {}; .stuff {}; }" => block[];
        "{ .stuff {}; $lark: {}; }" => block[];
        ".some-class:not(:last-child) {

            /* A comment! */
            $hello: ($a, $b) => $a + $b;
            $another: 2;

            $lark: {
                $sub1: 2px; /* another comment! */
            };

            $hello(2px, 5px);
            {
                border: 1px solid black;
                {
                    lark: another thing hereee;
                }
            }

            $bye.then;

            & .a-sub-class .more.another, .sub-clas-two {
                $subThing: -2px + 4px;
                color: red;
            }

            -moz-background-color:
                            1px solid blue; /* comments everywhere... */
            border-radius: 10px;

            $more: $lark.sub1;
        }" => block[];
    }

}

*/