use errors::errors::*;
use std::path::{Path,PathBuf};
use std::fs::File;
use std::io::Read;
use std::fmt::Write;
use std::iter;
use std::borrow::{Borrow,Cow};

pub struct Options<'a> {
    stdin: &'a str
}
impl <'a> Options<'a> {
    pub fn with_stdin(input: &str) -> Options {
        Options { stdin: input }
    }
}

pub fn display_error<'a>(e: ImportError, opts: Options<'a>) {
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
        CompileError(err, path) => {
            if path == PathBuf::new() {
                eprintln!("I ran into an error compiling from stdin:\n\n{}"
                    , display_compile_error(&err, &opts)
                )
            } else {
                eprintln!("I ran into an error compiling the file {}:\n\n{}"
                    , path.display()
                    , display_compile_error(&err, &opts)
                )
            }
        }
    }
}

// context provides the current path of the file that the error happened in.
// Each time we hit an import error, we recurse into it using the new path.
fn display_compile_error<'a>(err: &Error, opts: &Options<'a>) -> String {

    let mut out = match err.cause() {
        ErrorKind::ImportError(ImportError::CompileError(ref err, ..)) => {
            let mut o = display_compile_error(err, opts);
            o.push_str("\n");
            o
        },
        ErrorKind::ContextError(ContextError::At(ref err)) => {
            let mut o = display_compile_error(err, opts);
            o.push_str("\n");
            o
        },
        _ => {
            String::new()
        }
    };

    let at = err.at();

    let file_cow = if at.file() == &*PathBuf::new() && !opts.stdin.is_empty() {
        Cow::Borrowed(opts.stdin)
    } else {
        Cow::Owned(read_to_string(at.file()).unwrap_or(String::new()))
    };

    out.push_str(&err.error_summary());
    out.push_str(":\n\n");
    out.push_str(&highlight_error(&err.at(), file_cow.borrow())
        .unwrap_or_else(|| err.at().file().display().to_string()));
    out.push('\n');

    let desc = err.error_description();
    if !desc.is_empty() {
        out.push_str(&desc);
        out.push_str(".\n");
    }

    out
}

fn read_to_string<P: AsRef<Path>>(path: P) -> Option<String> {
    let mut file = String::new();
    File::open(path).ok()?.read_to_string(&mut file).ok()?;
    Some(file)
}

// print the relevant part of the file with the error location highlighted:
fn highlight_error(at: &At, file: &str) -> Option<String> {

    let by_lines: Vec<&str> = file.lines().collect();
    let Offsets{start_line, start_offset, end_line, end_offset} = get_lines_from_location(at.start(), at.end(), file);

    let max_line_num_length = (start_line+1..end_line+2).fold(0, |max,n| {
        max.max(n.to_string().len())
    });

    let mut out = String::new();
    let line_num_spaces = spaces(max_line_num_length);

    writeln!(&mut out, "{}--> {} ({}:{}-{}:{})"
        , line_num_spaces
        , at.file().display()
        // display 1 indexed values for humans:
        , start_line+1, start_offset+1, end_line+1, end_offset+1 ).unwrap();

    writeln!(&mut out, "{} |", line_num_spaces).unwrap();
    for line in start_line..end_line+1 {
        let line_human = line+1;
        let num_str = padded_num(line_human, max_line_num_length);
        let line_str = by_lines.get(line).unwrap_or(&"");

        writeln!(&mut out, "{} | {}", num_str, line_str).unwrap();

        if line == start_line {
            // cater for start and end offset being on same line, and for start offset
            // being at the end of the line (past it):
            let n = if start_line == end_line { end_offset.checked_sub(start_offset).unwrap_or(0).max(1) }
                    else { line_str.len().checked_sub(start_offset).unwrap_or(0).max(1) };

            let arrows: String = iter::repeat('^').take(n).collect();
            writeln!(&mut out, "{} | {}{}", line_num_spaces, spaces(start_offset), arrows).unwrap();
        } else if line > start_line && line < end_line {
            let arrows: String = iter::repeat('^').take(line_str.len()).collect();
            writeln!(&mut out, "{} | {}", line_num_spaces, arrows).unwrap();
        } else if line == end_line {
            // cater for position being off the end of the line.
            let n = line_str.len().checked_sub(end_offset).unwrap_or(0).max(1);
            let arrows: String = iter::repeat('^').take(n).collect();
            writeln!(&mut out, "{} | {}", line_num_spaces, arrows).unwrap();
        }
    }

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
fn get_lines_from_location(start: usize, end: usize, file: &str) -> Offsets {

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
