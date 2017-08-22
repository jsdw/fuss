#![macro_use]

/// generate an Err from an Expression and an ErrorType. This is a macro
/// so that it can pluck out only what it needs from the struct, rather than
/// trying to move the whole thing
macro_rules! err {
    ($e:ident, $err:expr) => ({
        Err(naked_err!($e, $err))
    })
}
macro_rules! naked_err {
    ($e:ident, $err:expr) => ({
        use std::path::PathBuf;
        let start = $e.start;
        let end = $e.end;
        Error{
            ty: $err,
            file: PathBuf::new(),
            start: start,
            end: end
        }
    })
}

// build an expression based on another expression without borrowing too much from it.
macro_rules! expression_from {
    ($e:ident, $expr:expr) => ({
        use types::*;
        Expression::with_position($e.start, $e.end, $expr)
    })
}

/// make it a little easier to build up a scope of primitive functions
macro_rules! scope {
    ( $($key:expr => $func:expr);+ ) => ({

        use std::collections::HashMap;
        use types::Scope;

        let mut map = HashMap::new();
        $(
            map.insert($key.to_owned(),
                Expression::new(Expr::PrimFunc(PrimFunc($func)))
            );
        )*
        Scope::from(map)

    })
}