use types::*;
use types::Primitive::*;
use std::fs::File;
use std::io::Read;
use evaluator::eval;
use parser::parse;
use std::path::PathBuf;

/// cast an expression to a boolean as best we can
pub fn import(args: &Vec<Expression>, context: &Context) -> PrimRes {
    if args.len() > 1 {
        return Err(ErrorType::WrongNumberOfArguments{ expected: 1, got: args.len() });
    }
    if !context.path.is_file() || !context.root.is_file() {
        return Err(ErrorType::CannotImportNoPathSet);
    }

    // expect a string input:
    let a = &args[0];
    let relpath = if let Expr::Prim(Str(ref relpath)) = a.expr {
        Ok(relpath)
    } else {
        Err(ErrorType::WrongTypeOfArguments{ message: "import requires a path string".to_owned() })
    }?;

    // is the path absolute? if so, search from wherever the root fuss file lives.
    let is_absolute = relpath.chars().take(1).collect::<String>() == "/".to_owned();

    // split the path into pieces, ignoring "." (stay in current dir) and "" (means more than one "/").
    let path_bits = relpath
        .split('/')
        .filter(|&s| s != "" && s != ".")
        .collect::<Vec<&str>>();

    // What path do we start at?
    let mut final_path = if is_absolute { context.root.clone() } else { context.path.clone() };
    // Remove the filename from it
    final_path.pop();
    // Traverse the import path to turn our starting path into our final one.
    for bit in &path_bits {
        if *bit == ".." {
            final_path.pop();
        } else {
            final_path.push(bit);
        }
    }

    // try to open the found file:
    import_path(&final_path, &context.root)

}

fn import_path(path: &PathBuf, root: &PathBuf) -> PrimRes {

    if let None = path.file_name() {
        return Err(ErrorType::CannotOpenFile(path.clone()));
    }

    let mut file = File::open(&path).or_else(|_| {
        //failed to open; try again setting extension to .fuss
        let mut filename = path.file_name().unwrap().to_owned();
        filename.push(".fuss");
        let new_path = path.with_file_name(&filename);
        File::open(&new_path)
    }).map_err(|_| ErrorType::CannotOpenFile(path.clone()))?;

    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents).map_err(|_| ErrorType::CannotReadFile(path.clone()))?;
    import_path_with_string(path, root, file_contents)

}

fn import_path_with_string(path: &PathBuf, root: &PathBuf, contents: String) -> PrimRes {

    let context = Context{
        path: path.clone(),
        root: root.clone()
    };

    // let res = parse(&input);
    let res = match parse(&contents) {
        Ok(expr) => eval(&expr, super::get_prelude(), &context).map_err(|mut e| {
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
       .map(|e| e.into_expr().expect("importing file: couldn't unwrap Rc"))
}

/// use this to eval a string into fuss; useful for interactive stuff but
/// can't import files if we didn't start with an actual filepath.
pub fn import_string(file: String) -> PrimRes {
    import_path_with_string(&PathBuf::new(), &PathBuf::new(), file)
}

/// our standard import mechanism, to get us going.
pub fn import_root(path: &PathBuf) -> PrimRes {
    import_path(path, path)
}