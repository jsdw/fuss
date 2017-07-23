#![macro_use]

/// generate an Err from an Expression and an ErrorType. This is a macro
/// so that it can pluck out only what it needs from the struct, rather than
/// trying to move the whole thing
macro_rules! err {
    ($e:ident, $err:expr) => ({
        use std::path::PathBuf;
        let start = $e.start;
        let end = $e.end;
        Err(Error{
            ty: $err,
            file: PathBuf::new(),
            start: start,
            end: end
        })
    })
}

// build an expression based on another expression without borrowing too much from it.
macro_rules! expression_from {
    ($e:ident, $expr:expr) => (
        Expression{
            start: $e.start,
            end: $e.end,
            expr: $expr
        }
    )
}

/// make it a little easier to build up a scope of primitive functions
macro_rules! scope {
    ( $($key:expr => $func:expr);+ ) => ({

        use std::collections::HashMap;
        use types::Scope;

        let mut map = HashMap::new();
        $(
            map.insert($key.to_owned(), Expression{
                start: Position::new(),
                end: Position::new(),
                expr: Expr::PrimFunc(PrimFunc($func))
            });
        )*
        Scope::from(map)

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