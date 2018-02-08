#![macro_use]

/// make it a little easier to build up a scope of primitive functions
macro_rules! scope {
    ( $($key:expr => $item:expr);+ ) => ({

        use std::collections::HashMap;
        use types::Scope;

        let mut map = HashMap::new();
        $(
            map.insert($key.to_owned(),
                EvaluatedExpression::new($item)
            );
        )*
        Scope::from(map)

    })
}