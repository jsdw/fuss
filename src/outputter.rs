use types::*;
use errors::*;
use std::io::{self, Write};

/// the only thing we expose from this:
pub fn print_css(block: EvaluatedBlock) -> Vec<Error> {

    let mut items = Items::new();
    items.populate_from_block(block);

    let mut s = String::new();
    let mut warnings = vec![];

    // print global fontfaces:
    for font in items.fontfaces {
        match font {
            Ok(block) => {
                fontface_into_string(0, block, &mut s);
            },
            Err(e) => {
                warnings.push(e);
            }
        };
    }

    // print global keyframe animations:
    for keyframe in items.keyframes {
        match keyframe {
            Ok(block) => {
                keyframes_into_string(0, block, &mut s);
            },
            Err(e) => {
                warnings.push(e);
            }
        }
    }

    // print CSS blocks:
    for m in items.media {

        let is_media = m.media.len() > 0;
        let indent = if is_media { 1 } else { 0 };

        if is_media {
            s += "@media ";
            s += &merge_media_query(m.media);
            s += " {\n";
        }

        if m.fontfaces.len() > 0 {
            for font in m.fontfaces {
                match font {
                    Ok(block) => {
                        fontface_into_string(indent, block, &mut s);
                    },
                    Err(e) => {
                        warnings.push(e);
                    }
                };
            }
        }

        if m.keyframes.len() > 0 {
            for keyframe in m.keyframes {
                match keyframe {
                    Ok(block) => {
                        keyframes_into_string(indent, block, &mut s);
                    },
                    Err(e) => {
                        warnings.push(e);
                    }
                }
            }
        }

        for style in m.styles {
            if style.selector.len() > 0 {
                css_block_into_string(indent, merge_css_selector(style.selector), style.keyvals, &mut s);
            } else if style.keyvals.len() > 0 {
                for KeyVals{at,..} in style.keyvals {
                    warnings.push(err(ShapeError::NakedKeyValNotAllowed, at))
                }
            }
        }

        if is_media {
            s += "}\n"
        }

    }

    let stdout = io::stdout();
    let mut handle = stdout.lock();
    handle.write_all(s.as_bytes()).expect("failed to write to stdout");
    warnings
}

fn keyframes_into_string(indent_count: usize, block: Keyframes, s: &mut String) {
    let indent: String = (0..indent_count).map(|_| '\t').collect();

    *s += &indent;
    *s += "@keyframes ";
    *s += &block.name;
    *s += " {\n";
    for section in block.inner {
        css_block_into_string(indent_count+1, section.selector, section.keyvals, s);
    }
    *s += &indent;
    *s += "}\n";
}

fn fontface_into_string(indent_count: usize, block: FontFace, s: &mut String) {
    let indent: String = (0..indent_count).map(|_| '\t').collect();

    *s += &indent;
    *s += "@font-face {\n";
    css_keyvals_into_string(indent_count+1, block.keyvals, s);
    *s += &indent;
    *s += "}\n";
}

fn css_block_into_string(indent_count: usize, selector: String, css: Vec<KeyVals>, s: &mut String) {
    let indent: String = (0..indent_count).map(|_| '\t').collect();

    *s += &indent;
    *s += selector.trim();
    *s += " {\n";
    css_keyvals_into_string(indent_count+1, css, s);
    *s += &indent;
    *s += "}\n";
}

fn css_keyvals_into_string(indent_count: usize, css: Vec<KeyVals>, s: &mut String) {
    let indent: String = (0..indent_count).map(|_| '\t').collect();

    for KeyVals{keyvals,..} in css {
        for KeyVal{key,val} in keyvals {
            *s += &indent;
            *s += key.trim();
            *s += ": ";
            *s += val.trim();
            *s += ";\n";
        }
    }
}

fn merge_media_query(query: Vec<String>) -> String {
    query.join(" and ")
}
fn merge_css_selector(mut selector: Vec<String>) -> String {

    selector.reverse();

    let mut current: Vec<String> = match selector.pop() {
        Some(val) => val.split(',').map(|s| s.trim().to_owned()).collect(),
        None => return String::new()
    };

    while let Some(next) = selector.pop() {

        let mut new_current = vec![];

        // if we have multiple selectors separated by ",",
        // apply each one independently to each selector we have so far.
        for next in next.split(',').map(|s| s.trim()) {

            for mut curr in current.iter().cloned() {

                // replace and '&'s in a selector with the previous, if any exist.
                // else, just append selectors separated by a space.
                if next.contains('&') {
                    curr = next.replace('&', &curr)
                } else {
                    curr.push(' ');
                    curr.push_str(next);
                }
                new_current.push(curr);

            }

        }
        current = new_current;

    }

    current.join(", ")
}

#[derive(Clone,PartialEq,Debug)]
struct Loc {
    media: Vec<String>,
    selector: Vec<String>,
    at: SmallVec<At>
}

#[derive(Clone,PartialEq,Debug)]
struct Media {
    media: Vec<String>,
    fontfaces: Vec<Result<FontFace,Error>>,
    styles: Vec<Style>,
    keyframes: Vec<Result<Keyframes,Error>>
}
impl Media {
    fn with_query(q: Vec<String>) -> Self { Media{media: q, fontfaces: vec![], keyframes: vec![], styles: vec![]} }
}

#[derive(Clone,PartialEq,Debug)]
struct Style {
    selector: Vec<String>,
    keyvals: Vec<KeyVals>
}
impl Style {
    fn with_selector(s: Vec<String>) -> Self { Style{selector: s, keyvals: vec![]} }
}

#[derive(Clone,PartialEq,Debug)]
struct Keyframes {
    name: String,
    inner: Vec<KeyframesInner>
}

#[derive(Clone,PartialEq,Debug)]
struct KeyframesInner {
    selector: String,
    keyvals: Vec<KeyVals>
}

#[derive(Clone,PartialEq,Debug)]
struct FontFace {
    keyvals: Vec<KeyVals>
}

#[derive(Clone,PartialEq,Debug)]
struct Items {
    media: Vec<Media>,
    fontfaces: Vec<Result<FontFace,Error>>,
    keyframes: Vec<Result<Keyframes,Error>>,
}

#[derive(Clone,PartialEq,Debug)]
struct KeyVals {
    at: SmallVec<At>,
    keyvals: Vec<KeyVal>
}

#[derive(Clone,PartialEq,Debug)]
struct KeyVal {
    key: String,
    val: String
}

impl Items {

    fn new() -> Items {
        Items {
            media: vec![],
            fontfaces: vec![],
            keyframes: vec![],
        }
    }

    fn populate_from_block(&mut self, block: EvaluatedBlock) {

        let at = block.at.clone();
        let entries = vec![ EvaluatedCSSEntry::Block(block) ];
        let loc = Loc{ media: vec![], selector: vec![], at: at };

        self.populate_from_entries(entries, loc);

    }

    fn populate_from_entries(&mut self, entries: Vec<EvaluatedCSSEntry>, loc: Loc) {

        let mut fontfaces = vec![];
        let mut keyvals = vec![];
        let mut keyframes = vec![];

        macro_rules! append_current{
            () => {
                if keyvals.len() > 0 || fontfaces.len() > 0 || keyframes.len() > 0 {

                    // get media block to append things to:
                    let mut media = if let Some(last_media) = self.media.pop() {
                        if last_media.media == loc.media {
                            last_media
                        } else {
                            let m = loc.media.clone();
                            self.media.push(last_media);
                            Media::with_query(m)
                        }
                    } else {
                        Media::with_query(loc.media.clone())
                    };

                    // get style block to append things to:
                    let mut style = if let Some(last_style) = media.styles.pop() {
                        if last_style.selector == loc.selector {
                            last_style
                        } else {
                            let s = loc.selector.clone();
                            media.styles.push(last_style);
                            Style::with_selector(s)
                        }
                    } else {
                        Style::with_selector(loc.selector.clone())
                    };

                    media.fontfaces.append(&mut fontfaces);
                    media.keyframes.append(&mut keyframes);

                    if !keyvals.is_empty() {
                        style.keyvals.push(KeyVals { at: loc.at.clone(), keyvals });
                        keyvals = vec![];
                        media.styles.push(style);
                    }
                    self.media.push(media);
                }
            }
        };

        for entry in entries {

            match entry {
                EvaluatedCSSEntry::KeyVal{key,val,..} => {
                    keyvals.push(KeyVal{key,val});
                },

                EvaluatedCSSEntry::Block(block) => {
                    let at = block.at.clone();
                    match block.ty {
                        BlockType::Keyframes => {

                            let k = if loc.media.len() == 0 { &mut self.keyframes } else { &mut keyframes };

                            k.push(match handle_keyframes(block) {
                                Err(e) => Err(err(e, at)),
                                Ok(v) => Ok(v)
                            });

                        },
                        BlockType::FontFace => {

                            let v = if loc.media.len() == 0 { &mut self.fontfaces } else { &mut fontfaces };

                            v.push(match handle_fontface(block) {
                                Err(e) => Err(err(e, at)),
                                Ok(v) => Ok(v)
                            });

                        },
                        BlockType::Media => {

                            append_current!();
                            let next_loc = Loc {
                                at: at,
                                media: { let mut m = loc.media.clone(); m.push(block.selector); m },
                                selector: loc.selector.clone()
                            };
                            self.populate_from_entries(block.css, next_loc);

                        },
                        BlockType::Generic => {

                            append_current!();
                            let next_loc = Loc {
                                at: at,
                                media: loc.media.clone(),
                                selector: {
                                    let mut s = loc.selector.clone();
                                    if block.selector.len() > 0 { s.push(block.selector); }
                                    s
                                }
                            };
                            self.populate_from_entries(block.css, next_loc);

                        }
                    }
                }
            }

        };

        append_current!();


    }

}

fn handle_keyframes(block: EvaluatedBlock) -> Result<Keyframes,ErrorKind> {

    let mut inner = vec![];
    for item in block.css {
        match item {
            EvaluatedCSSEntry::KeyVal{..} => {
                return ShapeError::KeyframesKeyvalsNotAllowedAtTop.into();
            },
            EvaluatedCSSEntry::Block(block) => {
                match block.ty {
                    BlockType::Keyframes => {
                        return ShapeError::KeyframesKeyframesBlockNotAllowed.into();
                    },
                    BlockType::FontFace => {
                        return ShapeError::KeyframesFontFaceBlockNotAllowed.into();
                    },
                    BlockType::Media => {
                        return ShapeError::KeyframesMediaBlockNotAllowed.into();
                    },
                    BlockType::Generic => {
                        let mut keyvals = vec![];
                        for entry in block.css {
                            match entry {
                                EvaluatedCSSEntry::KeyVal{key,val,..} => {
                                    // use the block's location for keyvals
                                    keyvals.push( KeyVal{key,val} );
                                },
                                EvaluatedCSSEntry::Block(..) => {
                                    return ShapeError::KeyframesNestedBlockNotAllowed.into();
                                }
                            }
                        }
                        inner.push(KeyframesInner{
                            selector: block.selector,
                            keyvals: vec![KeyVals{ at: block.at, keyvals: keyvals }]
                        });
                    }
                }
            }
        }
    }

    Ok(Keyframes{
        name: block.selector,
        inner: inner
    })

}
fn handle_fontface(block: EvaluatedBlock) -> Result<FontFace,ErrorKind> {

    let mut keyvals = vec![];
    for item in block.css {
        match item {
            EvaluatedCSSEntry::KeyVal{key,val,..} => {
                keyvals.push(KeyVal{key,val});
            },
            EvaluatedCSSEntry::Block(_) => {
                return ShapeError::FontfaceBlockNotAllowed.into();
            }
        }
    }

    Ok(FontFace{
        keyvals: vec![KeyVals{ at: block.at, keyvals: keyvals }]
    })

}
