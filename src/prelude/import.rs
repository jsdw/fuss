use types::*;
use errors::*;
use std::fs::File;
use std::io::Read;
use evaluator::eval;
use parser::parse;
use cache::Cache;
use std::rc::Rc;
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
    let relpath = if let EvaluatedExpr::Str(ref relpath) = *a.expr() {
        Ok(relpath)
    } else {
        Err(ApplicationError::WrongKindOfArguments{ index: 0, expected: vec![Kind::Str], got: a.expr().kind() })
    }?;

    // is the path absolute? if so, search from wherever the root fuss file lives.
    let is_absolute = relpath.chars().take(1).collect::<String>() == "/".to_owned();

    // split the path into pieces, ignoring "." (stay in current dir) and "" (means more than one "/").
    let path_bits = relpath
        .split('/')
        .filter(|&s| s != "" && s != ".")
        .collect::<Vec<&str>>();

    // What path do we start at?
    let mut final_path = if is_absolute { context.owned_root() } else { context.owned_path() };
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
        path: Rc::new(final_path),
        root: context.root.clone(),
        file_cache: context.file_cache.clone(),
        last: { let mut l = context.last.clone(); l.push(context.path.clone()); l }
    };

    // try to open the found file:
    import_path(new_context).map_err(|e| e.into())

}

fn import_path(mut context: Context) -> Result<EvaluatedExpr, ImportError> {

    context.path = {
        let mut p = context.owned_path();
        p.set_extension("fuss");
        Rc::new(p)
    };

    if context.last.iter().any(|p| p == &context.path) {
        return Err(ImportError::ImportLoop(context.owned_last(), context.owned_path()));
    }

    if let None = context.path.file_name() {
        return Err(ImportError::CannotOpenFile(context.owned_path()));
    }

    if context.file_cache.exists(&context.path) {
        return Ok( context.file_cache.get(&context.path).unwrap() );
    }

    let mut file = File::open(context.path_ref()).map_err(|_| ImportError::CannotOpenFile(context.owned_path()))?;

    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents).map_err(|_| ImportError::CannotReadFile(context.owned_path()))?;
    import_path_with_str(context, &file_contents)

}

fn import_path_with_str(context: Context, contents: &str) -> Result<EvaluatedExpr, ImportError> {
    parse(contents, &context)
        // eval parsed expr if aprsing is successful:
        .and_then(|expr| eval(&expr, super::get_prelude(), &context))
        // catch any parse/eval errors and wrap:
        .map_err(|e| ImportError::CompileError(e, context.owned_path()).into())
        // cache and return the final evaluatedexpr:
        .map(|e| {
            let expr = e.into_expr().expect("importing file: couldn't unwrap Rc");
            context.file_cache.set(context.path.clone(), expr.clone());
            expr
        })
}

/// use this to eval a string into fuss; useful for interactive stuff but
/// can't import files if we didn't start with an actual filepath.
pub fn import_string(file: &str) -> Result<EvaluatedExpr, ImportError> {

    let context = Context{
        path: Rc::new(PathBuf::new()),
        root: Rc::new(PathBuf::new()),
        file_cache: Cache::new(),
        last: Vec::new()
    };

    import_path_with_str(context, file)
}

/// our standard import mechanism, to get us going.
pub fn import_root(path: &PathBuf) -> Result<EvaluatedExpr, ImportError> {

    let context = Context{
        path: Rc::new(path.clone()),
        root: Rc::new(path.clone()),
        file_cache: Cache::new(),
        last: Vec::new()
    };

    import_path(context)
}