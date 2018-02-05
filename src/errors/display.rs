use errors::errors::*;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;
use std::fmt::Write;
use std::iter;

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
                    , display_compile_error(*boxed_err, &ErrorContext::new(path.clone()))
                )
            } else {
                eprintln!("I ran into an error compiling the file:\n\n{}"
                    , display_compile_error(*boxed_err, &ErrorContext::new(path.clone()))
                )
            }
        }
    }
}

// context provides the current path of the file that the error happened in.
// Each time we hit an import error, we recurse into it using the new path.
fn display_compile_error(err: Error, context: &ErrorContext) -> String {
    let mut out = if let ErrorKind::ImportError(ImportError::CompileError(ref inner_err, ref path)) = err.cause() {
        let mut o = display_compile_error(*inner_err.clone(), &ErrorContext::new(path.clone()));
        write!(&mut o, "\n\n");
        o
    } else {
        String::new()
    };

    write!(&mut out, "{}:\n\n{}\n{}"
        , err.error_summary()
        , highlight_error(&err.location(), context)
            .unwrap_or_else(|| context.path.display().to_string())
        , err.error_description()
    );

    out
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

    writeln!(&mut out, "{}--> {} ({}:{}-{}:{})"
        , line_num_spaces
        , context.path.display()
        , start_line, start_offset, end_line, end_offset );
    writeln!(&mut out, "{} |", line_num_spaces);
    for line in (start_line..end_line+1) {
        let line_human = line+1;
        let num_str = padded_num(line_human, max_line_num_length);
        let line_str = &by_lines[line];
        writeln!(&mut out, "{} | {}", num_str, line_str).ok()?;
        if line == start_line {
            // if error happens at end of line, don't overflow and just draw one arrow:
            let n = if line_str.len() <= start_offset { 1 } else { line_str.len() - start_offset };
            let arrows: String = iter::repeat('^').take(n).collect();
            writeln!(&mut out, "{} | {}{}", line_num_spaces, spaces(start_offset), arrows);
        } else if line > start_line && line < end_line {
            let arrows: String = iter::repeat('^').take(line_str.len()).collect();
            writeln!(&mut out, "{} | {}", line_num_spaces, arrows);
        } else if line == end_line {
            let n = if line_str.len() <= end_offset { 1 } else { line_str.len() - end_offset };
            let arrows: String = iter::repeat('^').take(n).collect();
            writeln!(&mut out, "{} | {}", line_num_spaces, arrows);
        }
    }
    writeln!(&mut out, "{} |", line_num_spaces);

    Some(out)
}

// returns a String consisting of n spaces
fn spaces(n: usize) -> String {
    iter::repeat(' ').take(n).collect()
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
    for (n, c) in file.char_indices().chain(iter::once((file.len(),' '))) {

        if n == start {
            start_line = lines_seen;
            start_offset = n - last_newline - 1;
        }
        if n == end {
            end_line = lines_seen;
            end_offset = n - last_newline - 1;
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