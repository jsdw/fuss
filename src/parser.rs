use types::{Expr, CSSEntry};
use chomp::prelude::*;
use chomp::parsers;

#[derive(PartialEq,Eq,Debug)]
enum MyError<I> {
    Err(I)
}

impl<I> From<parsers::Error<I>> for MyError<parsers::Error<I>> {
    fn from(e: parsers::Error<I>) -> MyError<parsers::Error<I>> {
        MyError::Err(e)
    }
}

type MyResult<'a,T> = ParseResult<&'a str,T,MyError<parsers::Error<char>>>;

fn skip_horizontal_spaces(i: &str) -> MyResult<()> {
    skip_while(i, |c| c == '\t' || c == ' ').map_err(MyError::Err)
}
fn skip_spaces(i: &str) -> MyResult<()> {
    skip_while(i, |c| c == '\t' || c == ' ' || c == '\n').map_err(MyError::Err)
}

fn css_key(i: &str) -> MyResult<String> {
    parse!{i;
        let v = take_while1(|c| c >= 'a' && c <= 'z' || c == '-');
        ret v.to_owned()
    }
}


fn css_keyval(i: &str) -> MyResult<CSSEntry> {
    parse!{i;
        let key = css_key();
            skip_horizontal_spaces();
            token(':');
            skip_horizontal_spaces();
        let val = take_till(|c| c == ';');
            token(';');
        ret CSSEntry::KeyVal{ key: key.to_owned(), val: val.to_owned() }
    }
}

fn css_entry(i: &str) -> MyResult<CSSEntry> {
    parse!{i;
        let pair = css_keyval();
        ret pair
    }
}

fn css_block(i: &str) -> MyResult<Vec<CSSEntry>> {
    parse!{i;
            token('{');
            skip_spaces();
        let keyvals = sep_by1(css_entry, skip_spaces);
            skip_spaces();
            token('}');
        ret keyvals;
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;

    #[test]
    fn test_css_key() {
        let res = parse_only_str(|i| css_key(i), "-hello-there : ");
        assert_eq!(res, Ok("-hello-there".to_owned()));
    }

    #[test]
    fn test_css_keyval() {
        let res = parse_only_str(|i| css_keyval(i), "-hello-there\t : you(123,456 );");
        assert_eq!(res, Ok(
            CSSEntry::KeyVal{ key: "-hello-there".to_owned(), val: "you(123,456 )".to_owned() }
        ));
    }

    #[test]
    fn test_css_keyval_compact() {
        let res = parse_only_str(|i| css_keyval(i), "-hello-there:you;");
        assert_eq!(res, Ok(
            CSSEntry::KeyVal{ key: "-hello-there".to_owned(), val: "you".to_owned() }
        ));
    }

    #[test]
    fn test_not_css_keyval_number() {
        let res = parse_only_str(|i| css_keyval(i), "-hell2o-there:you;").map_err(|_| ());
        assert_eq!(res, Err(()));
    }

    #[test]
    fn test_css_block() {
        let res = parse_only_str(|i| css_block(i), "{\n\t -hello-there\t : you; another: two;\n\n}");
        assert_eq!(res, Ok( vec![
            CSSEntry::KeyVal{ key: "-hello-there".to_owned(), val: "you".to_owned() },
            CSSEntry::KeyVal{ key: "another".to_owned(), val: "two".to_owned() }
        ]));
    }

}