use errors::errors::*;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;
use std::fmt::Write;
use std::iter::repeat;

pub fn display_error(e: ImportError) {
    use self::ImportError::*;
    match e {
        CannotImportNoPathSet | ImportLoop{..} => {
            unreachable!()
        },
        CannotOpenFile(path) => {
            eprintln!("I can't open the file:\n\n{}\n\nDoes it exist?", path.display())
        },
        CannotReadFile(path) => {
            eprintln!("I can't read the file:\n\n{}\n\nPerhaps you do not have permission?", path.display())
        },
        CompileError(boxed_err, path) => {
            if path == PathBuf::new() {
                eprintln!("I ran into an error compiling from stdin:\n\n{}"
                    , display_compile_error(boxed_err, &ErrorContext::new(path.clone()))
                )
            } else {
                eprintln!("I ran into an error compiling the file:\n\n{}"
                    , display_compile_error(boxed_err, &ErrorContext::new(path.clone()))
                )
            }
        }
    }
}

// context provides the current path of the file that the error happened in.
// Each time we hit an import error, we recurse into it using the new path.
fn display_compile_error(err: Box<Error>, context: &ErrorContext) -> String {
    format!("{}\n{}"
        , highlight_error(&err.location(), context).unwrap_or("Nope".to_owned())
        , err
    )
}

// print the relevant part of the file with the error location highlighted:
fn highlight_error(loc: &Location, context: &ErrorContext) -> Option<String> {

    let mut file = String::new();
    File::open(&context.path).ok()?.read_to_string(&mut file).ok()?;
    let by_lines: Vec<&str> = file.lines().collect();

    let Offsets{start_line, start_offset, end_line, end_offset} = get_lines_from_location(loc, &file);

    let max_line_num_length = (start_line+1..end_line+2).fold(0, |max,n| {
        max.max(n.to_string().len())
    });

    let mut out = String::new();
    let line_num_spaces = spaces(max_line_num_length);

    writeln!(&mut out, "{}--> {}", line_num_spaces, context.path.display());
    writeln!(&mut out, "{} |", line_num_spaces);
    for line in (start_line..end_line+1) {
        let line_human = line+1;
        let num_str = padded_num(line_human, max_line_num_length);
        let line_str = &by_lines[line];
        writeln!(&mut out, "{} | {}", num_str, line_str).ok()?;
        if line == start_line {
            let arrows: String = repeat('^').take(line_str.len() - start_offset).collect();
            writeln!(&mut out, "{} |{}{}", line_num_spaces, spaces(start_offset), arrows);
        } else if line > start_line && line < end_line {
            let arrows: String = repeat('^').take(line_str.len()).collect();
            writeln!(&mut out, "{} |{}", line_num_spaces, arrows);
        } else if line == end_line {
            let arrows: String = repeat('^').take(line_str.len() - end_offset).collect();
            writeln!(&mut out, "{} |{}", line_num_spaces, arrows);
        }
    }
    writeln!(&mut out, "{} |", line_num_spaces);

    Some(out)
}

// returns a String consisting of n spaces
fn spaces(n: usize) -> String {
    repeat(' ').take(n).collect()
}

// left-pad a number out to ensure the resulting string is always len in size
fn padded_num(num: usize, len: usize) -> String {
    let num_str = num.to_string();
    let num_len = num_str.len();
    if num_len < len {
        format!("{}{}", spaces(len - num_len), num_str)
    } else {
        num_str
    }
}

// given a start and end byte offset, we give back start and end line counts
// and offsets.
fn get_lines_from_location(loc: &Location, file: &str) -> Offsets {
    let start = loc.start();
    let end = loc.end();
    let mut start_line = 0;
    let mut start_offset = 0;
    let mut end_line = 0;
    let mut end_offset = 0;
    let mut last_newline = 0;
    let mut lines_seen = 0;
    for (n, c) in file.char_indices() {

        if n == start {
            start_line = lines_seen;
            start_offset = n - last_newline;
        }
        if n == end {
            end_line = lines_seen;
            end_offset = n - last_newline;
            break;
        }

        if c == '\n' {
            last_newline = n;
            lines_seen += 1;
        }
    }

    Offsets{start_line, start_offset, end_line, end_offset}
}

struct Offsets {
    start_line: usize,
    start_offset: usize,
    end_line: usize,
    end_offset: usize
}

struct ErrorContext {
    path: PathBuf
}
impl ErrorContext {
    fn new(path: PathBuf) -> ErrorContext {
        ErrorContext { path: path }
    }
}