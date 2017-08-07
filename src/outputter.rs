use types::*;
use std::vec;
use std::mem;


pub fn to_css(block: EvaluatedBlock) -> Result<String,Error> {
    let units = to_output_units(block, Location::new())?;
    Ok(unit_iter_to_string(UnitIterator::new(units)))
}

#[derive(Clone,Debug,PartialEq)]
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

#[derive(Clone,Debug,PartialEq)]
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
            out += &inner.0;
            out += ": ";
            out += &inner.1;
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

/// take an EvaluatedBlock and turn it into a vector of Units; our basic output blocks.
fn to_output_units(block: EvaluatedBlock, mut location: Location) -> Result<Vec<Unit>,Error> {

    // we are already in a font-face rule; not allowed any other blocks:
    if location.font {
        return err!(block, ErrorType::BlockNotAllowedInFontFace);
    }

    match block.block {
        Block::KeyframesBlock(b) => {
            if location.keyframes.is_some() {
                return err!(block, ErrorType::BlockNotAllowedInKeyframes);
            }
            location.keyframes = Some( (b.name, None) );
            handle_cssentries(location, b.inner)
        },
        Block::MediaBlock(b) => {
            if location.keyframes.is_some() {
                return err!(block, ErrorType::BlockNotAllowedInKeyframes);
            }
            location.media.push(b.query);
            handle_cssentries(location, b.css)
        },
        Block::FontFaceBlock(b) => {
            if location.keyframes.is_some() {
                return err!(block, ErrorType::BlockNotAllowedInKeyframes);
            }

            location.font = true;
            handle_cssentries(location, b.css)
        },
        Block::CSSBlock(b) => {

            let is_keyframes_inner = if let Some((_,Some(_))) = location.keyframes { true } else { false };
            let is_keyframes = if location.keyframes.is_some() { true } else { false };
            let selector = b.selector.trim().to_owned();
            let is_selector = selector.len() > 0;

            // we can't nest any further into keyframes blocks, so
            // if we seem to be trying to, bail out:
            if is_selector && is_keyframes_inner {
                return err!(block, ErrorType::BlockNotAllowedInKeyframes);
            }

            // if we have a selector and are in an @keyframes block, append to keyframes location,
            // else treat this is a normal css block and append to css entries. if no selector,
            // this unit will match the last, which is fine.
            if is_selector {
                if is_keyframes {
                    location.keyframes = location.keyframes.map(|mut o| { o.1 = Some(selector); o });
                } else {
                    location.css.push(selector);
                }
            }

            handle_cssentries(location, b.css)
        },
        _ => {
            // this should be accounted for already in eval stage:
            err!(block, ErrorType::NotACSSBlock)
        }
    }

}

/// turn a vector of evaluated CSSEntries into a vector of Units; our basic output blocks.
fn handle_cssentries(location: Location, entries: Vec<EvaluatedCSSEntry>) -> Result<Vec<Unit>,Error> {

    let mut output = vec![];
    let mut keyvals = vec![];

    for entry in entries {

        match entry {
            EvaluatedCSSEntry::KeyVal{key,val} => {
                keyvals.push( (key, val) );
            },
            EvaluatedCSSEntry::Block(block) => {

                if keyvals.len() > 0 {
                    output.push(Unit{
                        location: location.clone(),
                        keyvals: keyvals
                    });
                    keyvals = vec![];
                }

                let mut next_blocks = to_output_units(block, location.clone())?;
                output.append(&mut next_blocks);

            }
        }

    }

    if keyvals.len() > 0 {
        output.push(Unit{
            location: location.clone(),
            keyvals: keyvals
        });
    }

    Ok(output)

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
        let mut unit = match self.get_next() {
            None => return None,
            Some(unit) => unit
        };

        // try and append as many subsequent units as possible to it
        // as long as the location matches. put back the last one
        // we took out if it fails to match, so that we get it next time.
        while let Some(mut next_unit) = self.get_next() {

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
