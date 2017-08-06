use types::*;
use std::vec;
use std::mem;


pub fn to_css(block: Block) -> Result<String,Error> {

    let block_e = Expression{
        start: Position::new(0),
        end: Position::new(0),
        expr: Expr::Block(block)
    };

    let units = to_output_units(block_e, Location::new())?;
    Ok(unit_iter_to_string(UnitIterator::new(units)))

}

struct Location {
    // animation name, followed by position (eg 100%/from/to):
    keyframes: Option< (String, Option<String>) >,
    // media rules:
    media: Vec<String>,
    // css selectors:
    css: Vec<String>,
    // are we in a font-face (no rules etc; can't have anything else in font-face):
    font: bool
}

impl Location {
    fn new() -> Location {
        Location{
            keyframes: None,
            media: vec![],
            css: vec![],
            font: false
        }
    }
}

struct Unit {
    location: Location,
    keyvals: Vec<(String,String)>
}

fn unit_iter_to_string<I: Iterator<Item=Unit>>(iter: I) -> String {

    let mut out = String::new();
    for unit in iter {

        out += &make_selector_string(unit.location);
        out += " {\n";

        for inner in &unit.keyvals {
            out += "\t";
            out += inner.0;
            out += ": ";
            out += inner.1;
            out += "\n";
        }

        out += "}\n";

    }
    out

}

fn make_selector_string(loc: Location) -> String {

    // TODO need to handle non selectors here too
    let mut out = String::new();
    for s in &loc.css {
        out += s;
        out += " ";
    }
    out

}

fn to_output_units(expression: Expression, mut location: Location) -> Result<Vec<Unit>,Error> {

    // we are already in a font-face rule; not allowed any other blocks:
    if location.font {
        return err!(expression, ErrorType::BlockNotAllowedInFontFace);
    }

    match expression.expr {
        Expr::Block(Block::KeyframesBlock(b)) => {
            location.keyframes = Some( (b.name, None) );
            handle_cssentries(location, b.css)
        },
        Expr::Block(Block::MediaBlock(b)) => {
            location.media.push(b.query);
            handle_cssentries(location, b.css)
        },
        Expr::Block(Block::FontFaceBlock(b)) => {
            location.font = true;
            handle_cssentries(location, b.css)
        },
        Expr::Block(Block::CSSBlock(b)) => {
            let selector = b.selector.trim();
            if selector.len() > 0 {
                location.css.push(b.selector);
            }
            handle_cssentries(location, b.css)
        },
        _ => {
            // this should be accounted for already in eval stage:
            err!(expression, ErrorType::NotACSSBlock);
        }
    }

}

fn handle_cssentries(location: Location, entries: Vec<CSSEntry>) -> Result<Vec<Unit>,Error> {

    let output = vec![];
    let keyvals = vec![];

    for entry in entries {

        match entry {
            CSSEntry::KeyVal{key,val} => {
                keyvals.push( (key,val) );
            },
            CSSEntry::Expr(expression) => {

                if keyvals.len() {
                    output.push(Unit{
                        location: location.clone(),
                        keyvals: keyvals
                    });
                    keyvals = vec![];
                }

                let mut next_blocks = to_output_units(expression, location.clone())?;
                output.append(&mut next_blocks);

            }
        }

    }

    if keyvals.len() {
        output.push(Unit{
            location: location.clone(),
            keyvals: keyvals
        });
    }

    output

}

/// built form a vector of units; this iterates them,
/// aggregating identical units as it goes:
struct UnitIterator{
    iter: vec::IntoIter<Unit>,
    last: Option<Unit>
}

impl UnitIterator{
    fn new(units: Vec<Unit>) -> UnitIterator {
        UnitIterator{
            iter: units.into_iter(),
            last: None
        }
    }
    fn get_next(&mut self) -> Option<Unit> {
        if self.last.is_some() {
            let mut last = None;
            mem::swap(&mut self.last, &mut last);
            last
        } else {
            self.iter.next()
        }
    }
}

impl Iterator for UnitIterator {
    type Item = Unit;
    fn next(&mut self) -> Option<Unit> {

        // get the next unit, returning if there isn't one:
        let unit = match self.get_next() {
            None => return None,
            Some(unit) => unit
        };

        // try and append as many subsequent units as possible to it
        // as long as the location matches. put back the last one
        // we took out if it fails to match, so that we get it next time.
        while let Some(next_unit) = self.get_next() {

            if unit.location == next_unit.location {
                unit.keyvals.append(&mut next_unit.keyvals);
            } else {
                self.last = Some(next_unit);
                break;
            }

        }

        // hand back our aggregated unit:
        Some(unit)

    }
}
