use types::*;
use types::Primitive::*;
use std::fs::File;
use std::io::Read;
use evaluator::eval;
use parser::parse;
use std::path::PathBuf;
use std::collections::HashMap;

/// cast an expression to a boolean as best we can
pub fn import(mut args: Vec<Expression>, context: &Context) -> PrimRes {

    if args.len() > 1 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 1, got: args.len() });
    }

    // expect a string input:
    let a = args.remove(0);
    let relpath = if let Expr::Prim(Str(relpath)) = a.expr {
        Ok(relpath)
    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "import requires a path string".to_owned() })
    }?;

    // is the path absolute? if so, search from wherever the root fuss file lives.
    let is_absolute = relpath.chars().take(1).collect::<String>() == "/".to_owned();

    // split the path into pieces, ignoring "." (stay in current dir) and "" (means more than one "/").
    let path_bits = relpath
        .split('/')
        .filter(|&s| s == "" || s == ".")
        .collect::<Vec<&str>>();

    // What path do we start at?
    let mut final_path = if is_absolute { context.root.clone() } else { context.path.clone() };
    // Remove the filename from it
    final_path.pop();
    // Traverse the import path to turn our starting path into our final one.
    for bit in path_bits {
        if bit == ".." {
            final_path.pop();
        } else {
            final_path.push(bit);
        }
    }

    // try to open the found file:
    import_path(&final_path, &context.root)

}

pub fn import_path(path: &PathBuf, root: &PathBuf) -> PrimRes {

    let mut file_contents = String::new();
    let mut file = File::open(&path).map_err(|_| ErrorType::CannotOpenFile(path.clone()))?;
    file.read_to_string(&mut file_contents).map_err(|_| ErrorType::CannotReadFile(path.clone()))?;

    // try to parse and eval the contents:
    let context = Context{
        path: path.clone(),
        root: root.clone()
    };

    // let res = parse(&input);
    let res = match parse(&file_contents) {
        Ok(expr) => eval(expr, super::get_prelude(), &context).map_err(|mut e| {
            e.file = path.clone();
            e
        }),
        Err(err) => Err(Error{
            ty: err,
            file: path.clone(),
            start: Position::new(),
            end: Position::new()
        })
    };

    // return either the Expr or an ImportError which wraps the import issue.
    res.map_err(|e| ErrorType::ImportError(Box::new(e)))
        .map(|e| e.expr)

}