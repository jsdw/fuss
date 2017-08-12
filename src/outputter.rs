use types::*;
use std::vec;
use std::mem;

/// the only thing we expose from this:
pub fn print_css(block: EvaluatedBlock) {

    let unit_iter = UnitIterator::from_evaluated_block(block).filter_map(|item|{
        match item {
            Err(_) => None,
            Ok(unit) => Some(unit)
        }
    });

    for output_unit in OutputUnitIterator::from_unit_iterator(unit_iter) {

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
    Media{rule: String, inner: Vec<CSSOutputUnit>},
    KeyFrame{name: String, inner: Vec<CSSOutputUnit>},
    CSS(Vec<CSSOutputUnit>),
    FontFace{name: String, keyvals: Vec<(String,String)>}
}
struct CSSOutputUnit {
    selector: String,
    keyvals: Vec<(String,String)>
}

/// Iterate over an Iterator<Item=Unit> and output Result<OutputUnit,Error>'s until we run out.
struct OutputUnitIterator<Iter> {
    iter: Iter
}
impl <T> OutputUnitIterator<T> where T: Iterator<Item=Unit> {
    fn from_unit_iterator(iter: T) -> OutputUnitIterator<T> {
        OutputUnitIterator{iter:iter}
    }
}
impl <T> Iterator for OutputUnitIterator<T> where T: Iterator<Item=Unit> {
    type Item = OutputUnit;
    fn next(&mut self) -> Option<Self::Item> {

        None

    }
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
                // push keyvals to our list, remembering
                // that they will end up back to front:
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
                // since the original vector was reversed, the keyvals will
                // be back to front as well unless we flip them back around:
                keyvals: { keyvals.reverse(); keyvals }
            }))
        } else {
            self.next()
        }

    }
}

/// given a block and a location, output a new location (based on the block and last location) +
/// a new set of entries, or an error if something doesn't work out doing this.
fn make_stack_frame(block: EvaluatedBlock, mut location: Location) -> Result<UnitIteratorFrame,Error> {

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






























// /// Take an EvaluatedBlock and turn it into a vector of Units; our basic building blocks.
// /// For each type of block we run into, see if that block type is valid in the current
// /// locaiton and either error to that effect or continue on.
// fn block_to_units(block: EvaluatedBlock, mut location: Location) -> Result<Vec<Unit>,Error> {

//     match block.block {
//         Block::KeyframesBlock(b) => {
//             match location {
//                 CSS{..} => {
//                     handle_cssentries(Location::KeyframesOuter{ name: b.name }, b.inner)
//                 },
//                 KeyframesOuter{..} | KeyframesInner{..} => {
//                     err!(block, ErrorType::BlockNotAllowedInKeyframes)
//                 },
//                 FontFace => {
//                     err!(block, ErrorType::BlockNotAllowedInFontFace)
//                 }
//             }
//         },
//         Block::MediaBlock(b) => {
//             match location {
//                 CSS{mut media,css} => {
//                     media.push(b.query);
//                     handle_cssentries(Location::CSS{media,css}, b.css)
//                 },
//                 KeyframesOuter{..} | KeyframesInner{..} => {
//                     err!(block, ErrorType::BlockNotAllowedInKeyframes)
//                 },
//                 FontFace => {
//                     err!(block, ErrorType::BlockNotAllowedInFontFace)
//                 }
//             }
//         },
//         Block::FontFaceBlock(b) => {
//             match location {
//                 CSS{..} => {
//                     handle_cssentries(Location::FontFace, b.css)
//                 },
//                 KeyframesOuter{..} | KeyframesInner{..} => {
//                     err!(block, ErrorType::BlockNotAllowedInKeyframes)
//                 },
//                 FontFace => {
//                     err!(block, ErrorType::BlockNotAllowedInFontFace)
//                 }
//             }
//         },
//         Block::CSSBlock(b) => {

//             let selector = b.selector.trim().to_owned();
//             let is_selector = selector.len() > 0;

//             // no selector, so keep location the same as whatever it is
//             // and push the next unit, unless in a keyframe outer block,
//             // which can't have keyvals.
//             if !is_selector {
//                 if let KeyframesOuter{..} = location {
//                     return err!(block, ErrorType::BlockNotAllowedInKeyframes);
//                 } else {
//                     return handle_cssentries(location, b.css);
//                 }
//             }

//             match location {
//                 CSS{media,mut css} => {
//                     css.push(b.css);
//                     handle_cssentries(Location::CSS{media,css}, b.css)
//                 },
//                 KeyframesOuter{name} => {
//                     handle_cssentries(Location::KeyframesInner{name,inner:b.selector}, b.css)
//                 },
//                 KeyframesInner{..} => {
//                     err!(block, ErrorType::BlockNotAllowedInKeyframes)
//                 },
//                 FontFace => {
//                     err!(block, ErrorType::BlockNotAllowedInFontFace)
//                 }
//             }
//         }
//     }

// }

// /// take units and merge them into output units, complaining if we hit
// /// any disallowed things (like empty keyframe blocks)
// fn units_to_outputunits(Vec<Unit>) -> Result<Vec<Unit>,Error> {

// }














// #[derive(Clone,Debug,PartialEq)]
// struct Location {
//     // animation name, followed by position (eg 100%/from/to):
//     keyframes: Option< (String, Option<String>) >,
//     // media rules:
//     media: Vec<String>,
//     // css selectors:
//     css: Vec<String>,
//     // are we in a font-face (no rules etc; can't have anything else in font-face):
//     font: bool
// }

// impl Location {
//     fn new() -> Location {
//         Location{
//             keyframes: None,
//             media: vec![],
//             css: vec![],
//             font: false
//         }
//     }
// }

// pub fn to_css(block: EvaluatedBlock) -> Result<String,Error> {
//     let units = to_output_units(block, Location::new())?;
//     Ok(unit_iter_to_string(UnitIterator::new(units)))
// }

// fn unit_iter_to_string<I: Iterator<Item=Unit>>(iter: I) -> String {

//     let mut out = String::new();
//     for unit in iter {

//         out += &make_selector_string(unit.location);
//         out += " {\n";

//         for inner in &unit.keyvals {
//             out += "\t";
//             out += &inner.0;
//             out += ": ";
//             out += &inner.1;
//             out += "\n";
//         }

//         out += "}\n";

//     }
//     out

// }

// fn make_selector_string(loc: Location) -> String {

//     // TODO need to handle non selectors here too
//     let mut out = String::new();

//     // if we are in an @font-face rule, ignore everything else in location:
//     if loc.font {
//         return "@font-face".to_owned();
//     }

//     // if we have media queries present, output them first, concat with 'and'
//     if loc.media.len() > 0 {
//         out += "@media ";
//         out += &loc.media.join(" and ");
//     }

//     // append any CSS
//     for s in &loc.css {
//         out += s;
//         out += " ";
//     }
//     out

// }

// /// take an EvaluatedBlock and turn it into a vector of Units; our basic output blocks.
// fn to_output_units(block: EvaluatedBlock, mut location: Location) -> Result<Unit,Error> {

//     // we are already in a font-face rule; not allowed any other blocks:
//     if location.font {
//         return err!(block, ErrorType::BlockNotAllowedInFontFace);
//     }

//     match block.block {
//         Block::KeyframesBlock(b) => {
//             if location.keyframes.is_some() {
//                 return err!(block, ErrorType::BlockNotAllowedInKeyframes);
//             }
//             location.keyframes = Some( (b.name, None) );
//             handle_cssentries(location, b.inner)
//         },
//         Block::MediaBlock(b) => {
//             if location.keyframes.is_some() {
//                 return err!(block, ErrorType::BlockNotAllowedInKeyframes);
//             }
//             location.media.push(b.query);
//             handle_cssentries(location, b.css)
//         },
//         Block::FontFaceBlock(b) => {
//             if location.keyframes.is_some() {
//                 return err!(block, ErrorType::BlockNotAllowedInKeyframes);
//             }

//             location.font = true;
//             handle_cssentries(location, b.css)
//         },
//         Block::CSSBlock(b) => {

//             let is_keyframes_inner = if let Some((_,Some(_))) = location.keyframes { true } else { false };
//             let is_keyframes = if location.keyframes.is_some() { true } else { false };
//             let selector = b.selector.trim().to_owned();
//             let is_selector = selector.len() > 0;

//             // we can't nest any further into keyframes blocks, so
//             // if we seem to be trying to, bail out:
//             if is_selector && is_keyframes_inner {
//                 return err!(block, ErrorType::BlockNotAllowedInKeyframes);
//             }

//             // if we have a selector and are in an @keyframes block, append to keyframes location,
//             // else treat this is a normal css block and append to css entries. if no selector,
//             // this unit will match the last, which is fine.
//             if is_selector {
//                 if is_keyframes {
//                     location.keyframes = location.keyframes.map(|mut o| { o.1 = Some(selector); o });
//                 } else {
//                     location.css.push(selector);
//                 }
//             }

//             handle_cssentries(location, b.css)
//         }
//     }

// }

// /// turn a vector of evaluated CSSEntries into a vector of Units; our basic output blocks.
// fn handle_cssentries(location: Location, entries: Vec<EvaluatedCSSEntry>) -> Result<Vec<Unit>,Error> {

//     let mut output = vec![];
//     let mut keyvals = vec![];

//     for entry in entries {

//         match entry {
//             EvaluatedCSSEntry::KeyVal{key,val} => {
//                 keyvals.push( (key, val) );
//             },
//             EvaluatedCSSEntry::Block(block) => {

//                 if keyvals.len() > 0 {
//                     output.push(Unit{
//                         location: location.clone(),
//                         keyvals: keyvals
//                     });
//                     keyvals = vec![];
//                 }

//                 let mut next_blocks = to_output_units(block, location.clone())?;
//                 output.append(&mut next_blocks);

//             }
//         }

//     }

//     if keyvals.len() > 0 {
//         output.push(Unit{
//             location: location.clone(),
//             keyvals: keyvals
//         });
//     }

//     Ok(output)

// }

// /// built form a vector of units; this iterates them,
// /// aggregating identical units as it goes:
// struct UnitIterator{
//     iter: vec::IntoIter<Unit>,
//     last: Option<Unit>
// }

// impl UnitIterator{
//     fn new(units: Vec<Unit>) -> UnitIterator {
//         UnitIterator{
//             iter: units.into_iter(),
//             last: None
//         }
//     }
//     fn get_next(&mut self) -> Option<Unit> {
//         if self.last.is_some() {
//             let mut last = None;
//             mem::swap(&mut self.last, &mut last);
//             last
//         } else {
//             self.iter.next()
//         }
//     }
// }

// impl Iterator for UnitIterator {
//     type Item = Unit;
//     fn next(&mut self) -> Option<Unit> {

//         // get the next unit, returning if there isn't one:
//         let mut unit = match self.get_next() {
//             None => return None,
//             Some(unit) => unit
//         };

//         // try and append as many subsequent units as possible to it
//         // as long as the location matches. put back the last one
//         // we took out if it fails to match, so that we get it next time.
//         while let Some(mut next_unit) = self.get_next() {

//             if unit.location == next_unit.location {
//                 unit.keyvals.append(&mut next_unit.keyvals);
//             } else {
//                 self.last = Some(next_unit);
//                 break;
//             }

//         }

//         // hand back our aggregated unit:
//         Some(unit)

//     }
// }
