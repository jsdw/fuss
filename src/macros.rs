#![macro_use]

// build an expression based on another expression without borrowing too much from it.
macro_rules! expression_from {
    ($e:ident, $expr:expr) => ({
        use types::*;
        ExpressionOuter::with_position($e.start, $e.end, $expr)
    })
}

/// make it a little easier to build up a scope of primitive functions
macro_rules! scope {
    ( $($key:expr => $item:expr);+ ) => ({

        use std::collections::HashMap;
        use types::Scope;

        let mut map = HashMap::new();
        $(
            map.insert($key.to_owned(),
                ExpressionOuter::new($item)
            );
        )*
        Scope::from(map)

    })
}