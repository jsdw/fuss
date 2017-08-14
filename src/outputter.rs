use types::*;
use std::iter::Peekable;
use std::io::{self, Write};

/// the only thing we expose from this:
pub fn print_css(block: EvaluatedBlock) {

    let stdout = io::stdout();
    let mut handle = stdout.lock();

    let unit_iter = UnitIterator::from_evaluated_block(block).filter_map(|item|{
        match item {
            Err(e) => { eprintln!("Warning: {:?}", e); None },
            Ok(unit) => Some(unit)
        }
    });

    for output_unit in OutputUnitIterator::from_unit_iterator(unit_iter) {
        if let Err(e) = handle.write_all(output_unit_to_string(output_unit).as_bytes()) {
            eprintln!("Error writing to output: {}", e);
        }
    }

}

fn output_unit_to_string(unit: OutputUnit) -> String {
    let mut s = String::new();
    match unit {
        OutputUnit::CSS{media, inner} => {
            if !media.is_empty() {
                s += "@media ";
                s += &media;
                s += " {\n";
                css_output_units_into_string(1, inner, &mut s);
                s += "}\n";
            } else {
                css_output_units_into_string(0, inner, &mut s);
            }
        },
        OutputUnit::KeyFrame{name, inner} => {
            s += "@keyframes ";
            s += &name;
            s += " {\n";
            css_output_units_into_string(1, inner, &mut s);
            s += "}\n";
        }
        OutputUnit::FontFace(keyvals) => {
            s += "@font-face {\n";
            css_keyvals_into_string(1, keyvals, &mut s);
            s += "}\n";
        }
    }
    s
}

fn css_output_units_into_string(indent_count: usize, css: Vec<CSSOutputUnit>, s: &mut String) {
    for unit in css {
        css_output_unit_into_string(indent_count, unit, s);
    }
}

fn css_output_unit_into_string(indent_count: usize, css: CSSOutputUnit, s: &mut String) {
    let indent: String = (0..indent_count).map(|_| '\t').collect();

    *s += &indent;
    *s += &css.selector;
    *s += " {\n";
    css_keyvals_into_string(indent_count+1, css.keyvals, s);
    *s += &indent;
    *s += "}\n"
}

fn css_keyvals_into_string(indent_count: usize, css: Vec<(String,String)>, s: &mut String) {
    let indent: String = (0..indent_count).map(|_| '\t').collect();

    for (key,val) in css {
        *s += &indent;
        *s += &key;
        *s += ": ";
        *s += &val;
        *s += ";\n";
    }
}


/// represent the current location, disallowing invalid states as best we can:
#[derive(Clone,Debug,PartialEq)]
enum Location {
    CSS{ media: Vec<String>, css: Vec<String> },
    FontFace,
    KeyframesOuter{ name: String },
    KeyframesInner{ name: String, inner: String }
}
impl Location {
    fn new() -> Location {
        Location::CSS{ media: vec![], css: vec![] }
    }
}

/// a Unit is our basic building block; it represents some keyval pairs at some location.
/// Units can be outputted as is, but better to aggregate them by first merging media
/// queries together, and then merging css rules together.
#[derive(Clone,Debug,PartialEq)]
struct Unit {
    location: Location,
    keyvals: Vec<(String,String)>
}

/// We transform Unit into this to ensure that there are no issues, and provide the right
/// nesting and aggregation of things:
enum OutputUnit {
    CSS{media: String, inner: Vec<CSSOutputUnit>},
    KeyFrame{name: String, inner: Vec<CSSOutputUnit>},
    FontFace(Vec<(String,String)>)
}
struct CSSOutputUnit {
    selector: String,
    keyvals: Vec<(String,String)>
}

/// Iterate over an Iterator<Item=Unit> and output Result<OutputUnit,Error>'s until we run out.
struct OutputUnitIterator<Iter: Iterator<Item=Unit>> {
    iter: Peekable<Iter>
}
impl <T> OutputUnitIterator<T> where T: Iterator<Item=Unit> {
    fn from_unit_iterator(iter: T) -> OutputUnitIterator<T> {
        OutputUnitIterator{iter:iter.peekable()}
    }
}
impl <T> Iterator for OutputUnitIterator<T> where T: Iterator<Item=Unit> {
    type Item = OutputUnit;
    fn next(&mut self) -> Option<Self::Item> {

        let unit = match self.iter.next() {
            None => return None,
            Some(unit) => unit
        };

        match unit.location {
            Location::CSS{media,css} => {

                let mut output_units = vec![CSSOutputUnit{
                    selector: css.join(" "),
                    keyvals: unit.keyvals
                }];

                loop {

                    // loop as long as we find another CSS unit with matching media query:
                    if let Some(&Unit{location: Location::CSS{ media: ref media2,..},..}) = self.iter.peek() {
                        if *media2 != media { break; }
                    } else {
                        break;
                    }

                    let other_unit = self.iter.next().unwrap();
                    let selector = match other_unit.location {
                        Location::CSS{css,..} => css,
                        _ => unreachable!("should be Location::CSS; checked above")
                    }.join(" ");

                    // merge css with existing output units:
                    push_to_css_output_units(
                        &mut output_units,
                        CSSOutputUnit{
                            selector: selector,
                            keyvals: other_unit.keyvals
                        }
                    );

                }

                Some(OutputUnit::CSS{
                    media: media.join(" and "),
                    inner: output_units
                })

            },
            Location::FontFace => {

                // doesn't currently handle merging empty blocks inside these,
                // since we may have different font faces next to eachother.
                // need to merge them before we get this far.
                Some(OutputUnit::FontFace(unit.keyvals))

            },
            Location::KeyframesOuter{..} => {

                // ignore this; should have been picked up earlier, and is not valid
                // to have css at this level
                self.next()

            },
            Location::KeyframesInner{name,inner} => {

                let mut output_units = vec![CSSOutputUnit{
                    selector: inner,
                    keyvals: unit.keyvals
                }];

                loop {

                    // loop as long as we find another KeyframesInner unit with matching name:
                    if let Some(&Unit{location: Location::KeyframesInner{ name: ref name2,..},..}) = self.iter.peek() {
                        if *name2 != name { break; }
                    } else {
                        break;
                    }

                    let other_unit = self.iter.next().unwrap();
                    let selector = match other_unit.location {
                        Location::KeyframesInner{inner,..} => inner,
                        _ => unreachable!("should be Location::KeyframesInner; checked above")
                    };

                    // merge css with existing output units:
                    push_to_css_output_units(
                        &mut output_units,
                        CSSOutputUnit{
                            selector: selector,
                            keyvals: other_unit.keyvals
                        }
                    );

                }

                Some(OutputUnit::KeyFrame{
                    name: name,
                    inner: output_units
                })

            }
        }

    }
}

fn push_to_css_output_units(units: &mut Vec<CSSOutputUnit>, mut unit: CSSOutputUnit) {
    if let Some(last) = units.last_mut() {
        if last.selector == unit.selector {
            last.keyvals.append(&mut unit.keyvals);
            return;
        }
    }
    units.push(unit);
}

/// Iterate over an EvaluatedBlock, outputting Result<Unit,Error>'s until we run out.
struct UnitIterator {
    stack: Vec<UnitIteratorFrame>
}
type UnitIteratorFrame = (Vec<EvaluatedCSSEntry>,Location);
impl UnitIterator {
    fn from_evaluated_block(block: EvaluatedBlock) -> UnitIterator {
        UnitIterator{
            stack: vec![ (vec![EvaluatedCSSEntry::Block(block)], Location::new()) ]
        }
    }
}
impl Iterator for UnitIterator {
    type Item = Result<Unit,Error>;
    fn next(&mut self) -> Option<Self::Item> {

        let (mut top,location) = match self.stack.pop() {
            None => return None,
            Some(s) => s
        };

        // short circuit if we have nout to look at
        // in our current stack frame, and try again
        // the next frame down:
        if top.len() == 0 {
            return self.next();
        }

        let mut keyvals = vec![];
        while let Some(entry) = top.pop() {

            match entry {
                // push keyvals to our list. since they are already
                // back to front, they will end up in the right order
                // again in keyvals:
                EvaluatedCSSEntry::KeyVal{key,val} => {
                    keyvals.push( (key,val) );
                },
                // if we hit a block, add the block to our stack
                // (and replace the current top if needed to keep
                // the stack in order), then break, as we're done for now.
                EvaluatedCSSEntry::Block(block) => {
                    if top.len() > 0 { self.stack.push( (top,location.clone()) ); }
                    match make_stack_frame(block, location.clone()) {
                        Err(err) => return Some(Err(err)),
                        Ok(frame) => self.stack.push(frame)
                    };
                    break;
                }
            }

        }

        if keyvals.len() > 0 {
            Some(Ok(Unit{
                location: location.clone(),
                keyvals: keyvals
            }))
        } else {
            self.next()
        }

    }
}

/// given a block and a location, output a new location (based on the block and last location) +
/// a new set of entries, or an error if something doesn't work out doing this.
fn make_stack_frame(block: EvaluatedBlock, location: Location) -> Result<UnitIteratorFrame,Error> {

    use self::Location::*;

    // reverse the entries so that we can work backwards, easily popping them.
    fn to_frame(mut css: Vec<EvaluatedCSSEntry>, loc: Location) -> Result<UnitIteratorFrame,Error> {
        css.reverse();
        Ok( (css, loc) )
    }

    match block.block {
        Block::KeyframesBlock(b) => {
            match location {
                CSS{..} => {
                    to_frame(b.inner, Location::KeyframesOuter{ name: b.name })
                },
                KeyframesOuter{..} | KeyframesInner{..} => {
                    err!(block, ErrorType::BlockNotAllowedInKeyframes)
                },
                FontFace => {
                    err!(block, ErrorType::BlockNotAllowedInFontFace)
                }
            }
        },
        Block::MediaBlock(b) => {
            match location {
                CSS{mut media,css} => {
                    media.push(b.query);
                    to_frame(b.css, Location::CSS{media,css})
                },
                KeyframesOuter{..} | KeyframesInner{..} => {
                    err!(block, ErrorType::BlockNotAllowedInKeyframes)
                },
                FontFace => {
                    err!(block, ErrorType::BlockNotAllowedInFontFace)
                }
            }
        },
        Block::FontFaceBlock(b) => {
            match location {
                CSS{..} => {
                    to_frame(b.css, Location::FontFace)
                },
                KeyframesOuter{..} | KeyframesInner{..} => {
                    err!(block, ErrorType::BlockNotAllowedInKeyframes)
                },
                FontFace => {
                    err!(block, ErrorType::BlockNotAllowedInFontFace)
                }
            }
        },
        Block::CSSBlock(b) => {

            let selector = b.selector.trim().to_owned();
            let is_selector = selector.len() > 0;

            // no selector, so keep location the same as whatever it is
            // and push the next unit, unless in a keyframe outer block,
            // which can't have keyvals.
            if !is_selector {
                if let KeyframesOuter{..} = location {
                    return err!(block, ErrorType::BlockNotAllowedInKeyframes);
                } else {
                    return to_frame(b.css, location);
                }
            }

            match location {
                CSS{media,mut css} => {
                    css.push(b.selector);
                    to_frame(b.css, Location::CSS{media,css})
                },
                KeyframesOuter{name} => {
                    to_frame(b.css, Location::KeyframesInner{name,inner:b.selector})
                },
                KeyframesInner{..} => {
                    err!(block, ErrorType::BlockNotAllowedInKeyframes)
                },
                FontFace => {
                    err!(block, ErrorType::BlockNotAllowedInFontFace)
                }
            }
        }
    }

}
