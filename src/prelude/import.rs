use types::*;
use errors::*;
use std::fs::File;
use std::io::Read;
use evaluator::eval;
use parser::parse;
use cache::Cache;
use std::path::PathBuf;

/// cast an expression to a boolean as best we can
pub fn import(args: &Vec<EvaluatedExpression>, context: &Context) -> PrimRes {

    if args.len() > 1 {
        return ApplicationError::WrongNumberOfArguments{ expected: 1, got: args.len() }.into()
    }
    if !context.path.is_file() || !context.root.is_file() {
        return ImportError::CannotImportNoPathSet.into()
    }

    // expect a string input:
    let a = &args[0];
    let relpath = if let EvaluatedExpr::Str(ref relpath) = a.expr {
        Ok(relpath)
    } else {
        Err(ApplicationError::WrongKindOfArguments{ index: 0, expected: vec![Kind::Str], got: a.expr.kind() })
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

    let new_context = Context{
        path: final_path,
        root: context.root.clone(),
        file_cache: context.file_cache.clone(),
        last: { let mut l = context.last.clone(); l.push(context.path.clone()); l }
    };

    // try to open the found file:
    import_path(new_context)

}

fn import_path(mut context: Context) -> PrimRes {

    context.path.set_extension("fuss");

    if context.last.iter().any(|p| p == &context.path) {
        return ImportError::ImportLoop(context.last.clone(), context.path.clone()).into()
    }

    if let None = context.path.file_name() {
        return ImportError::CannotOpenFile(context.path.clone()).into();
    }

    if context.file_cache.exists(&context.path) {
        return Ok( context.file_cache.get(&context.path).unwrap() );
    }

    let mut file = File::open(&context.path).map_err(|_| ImportError::CannotOpenFile(context.path.clone()))?;

    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents).map_err(|_| ImportError::CannotReadFile(context.path.clone()))?;
    import_path_with_string(context, file_contents)

}

fn import_path_with_string(context: Context, contents: String) -> PrimRes {

    // let res = parse(&input);
    let res = match parse(&contents) {
        Ok(expr) => eval(&expr, super::get_prelude(), &context),//.map_err(|mut e| e.file(context.path.clone())),
        Err(e) => Err(err(e, Location::at(0,0).file(context.path.clone())))
    };

    // return either the Expr or an ImportError which wraps the import issue.
    res.map_err(|e| ImportError::Import(Box::new(e)).into())
       .map(|e| {
           let expr = e.into_expr().expect("importing file: couldn't unwrap Rc");
           context.file_cache.set(context.path.clone(), expr.clone());
           expr
       })
}

/// use this to eval a string into fuss; useful for interactive stuff but
/// can't import files if we didn't start with an actual filepath.
pub fn import_string(file: String) -> PrimRes {

    let context = Context{
        path: PathBuf::new(),
        root: PathBuf::new(),
        file_cache: Cache::new(),
        last: Vec::new()
    };

    import_path_with_string(context, file)
}

/// our standard import mechanism, to get us going.
pub fn import_root(path: &PathBuf) -> PrimRes {

    let context = Context{
        path: path.clone(),
        root: path.clone(),
        file_cache: Cache::new(),
        last: Vec::new()
    };

    import_path(context)
}