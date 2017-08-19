use types::*;
use std::io::{self, Write};

/// the only thing we expose from this:
pub fn print_css(block: EvaluatedBlock) {

    let mut items = Items::new();
    items.populate_from_block(block);

    let mut s = String::new();

    // print global fontfaces:
    for font in items.fontfaces {
        match font {
            Ok(block) => {
                fontface_into_string(0, block, &mut s);
            },
            Err(e) => {
                eprintln!("Warning (font-face): {:?}", e);
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
                eprintln!("Warning (keyframes): {:?}", e);
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
                        eprintln!("Warning: {:?}", e);
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
                        eprintln!("Warning (keyframes): {:?}", e);
                    }
                }
            }
        }

        for style in m.styles {
            if style.selector.len() > 0 {
                css_block_into_string(indent, merge_css_selector(style.selector), style.keyvals, &mut s);
            } else if style.keyvals.len() > 0 {
                eprintln!("Warning: can't have naked keyvals")
            }
        }

        if is_media {
            s += "}\n"
        }

    }

    let stdout = io::stdout();
    let mut handle = stdout.lock();
    handle.write_all(s.as_bytes());

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

fn css_block_into_string(indent_count: usize, selector: String, css: Vec<(String,String)>, s: &mut String) {
    let indent: String = (0..indent_count).map(|_| '\t').collect();

    *s += &indent;
    *s += &selector;
    *s += " {\n";
    css_keyvals_into_string(indent_count+1, css, s);
    *s += &indent;
    *s += "}\n";
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

fn merge_media_query(query: Vec<String>) -> String {
    query.join(" and ")
}
fn merge_css_selector(mut selector: Vec<String>) -> String {

    selector.reverse();

    let mut current = match selector.pop() {
        Some(val) => val,
        None => return String::new()
    };

    while let Some(next) = selector.pop() {

        // replace and '&'s in a selector with the previous, if any exist.
        // else, just append selectors separated by a space.
        if next.contains('&') {
            current = next.replace('&', &current);
        } else {
            current.push(' ');
            current.push_str(&next);
        }
    }

    current
}

#[derive(Clone,PartialEq,Debug)]
struct Loc {
    media: Vec<String>,
    selector: Vec<String>
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
    keyvals: Vec<(String,String)>
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
    keyvals: Vec<(String,String)>
}

#[derive(Clone,PartialEq,Debug)]
struct FontFace {
    keyvals: Vec<(String,String)>
}

#[derive(Clone,PartialEq,Debug)]
struct Items {
    media: Vec<Media>,
    fontfaces: Vec<Result<FontFace,Error>>,
    keyframes: Vec<Result<Keyframes,Error>>,
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

        let entries = vec![ EvaluatedCSSEntry::Block(block) ];
        let loc = Loc{ media: vec![], selector: vec![] };

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
                    style.keyvals.append(&mut keyvals);
                    if style.keyvals.len() > 0 { media.styles.push(style); }
                    self.media.push(media);
                }
            }
        };

        for entry in entries {

            match entry {
                EvaluatedCSSEntry::KeyVal{key,val} => {
                    keyvals.push( (key,val) );
                },

                EvaluatedCSSEntry::Block(block) => {

                    match block.block {
                        Block::KeyframesBlock(b) => {

                            let k = if loc.media.len() == 0 { &mut self.keyframes } else { &mut keyframes };

                            k.push(match handle_keyframes(b) {
                                Err(e) => err!(block, e),
                                Ok(v) => Ok(v)
                            });

                        },
                        Block::FontFaceBlock(b) => {

                            let v = if loc.media.len() == 0 { &mut self.fontfaces } else { &mut fontfaces };

                            v.push(match handle_fontface(b) {
                                Err(e) => err!(block, e),
                                Ok(v) => Ok(v)
                            });

                        },
                        Block::MediaBlock(b) => {

                            append_current!();
                            let next_loc = Loc {
                                media: { let mut m = loc.media.clone(); m.push(b.query); m },
                                selector: loc.selector.clone()
                            };
                            self.populate_from_entries(b.css, next_loc);

                        },
                        Block::CSSBlock(b) => {

                            append_current!();
                            let next_loc = Loc {
                                media: loc.media.clone(),
                                selector: {
                                    let mut s = loc.selector.clone();
                                    if b.selector.len() > 0 { s.push(b.selector); }
                                    s
                                }
                            };
                            self.populate_from_entries(b.css, next_loc);

                        }
                    }
                }
            }

        };

        append_current!();


    }

}

fn handle_keyframes(block: EvaluatedKeyframesBlock) -> Result<Keyframes,ErrorType> {

    let mut inner = vec![];
    for item in block.inner {
        match item {
            EvaluatedCSSEntry::KeyVal{..} => {
                return Err(ErrorType::Keyframes_KeyvalsNotAllowedAtTop);
            },
            EvaluatedCSSEntry::Block(block) => {
                match block.block {
                    Block::KeyframesBlock(..) => {
                        return Err(ErrorType::Keyframes_KeyframesBlockNotAllowed);
                    },
                    Block::FontFaceBlock(..) => {
                        return Err(ErrorType::Keyframes_FontFaceBlockNotAllowed);
                    },
                    Block::MediaBlock(..) => {
                        return Err(ErrorType::Keyframes_MediaBlockNotAllowed);
                    },
                    Block::CSSBlock(b) => {
                        let mut keyvals = vec![];
                        for entry in b.css {
                            match entry {
                                EvaluatedCSSEntry::KeyVal{key,val} => {
                                    keyvals.push( (key,val) );
                                },
                                EvaluatedCSSEntry::Block(..) => {
                                    return Err(ErrorType::Keyframes_NestedBlockNotAllowed);
                                }
                            }
                        }
                        inner.push(KeyframesInner{
                            selector: b.selector,
                            keyvals: keyvals
                        });
                    }
                }
            }
        }
    }

    Ok(Keyframes{
        name: block.name,
        inner: inner
    })

}
fn handle_fontface(block: EvaluatedFontFaceBlock) -> Result<FontFace,ErrorType> {

    let mut keyvals = vec![];
    for item in block.css {
        match item {
            EvaluatedCSSEntry::KeyVal{key,val} => {
                keyvals.push( (key,val) );
            },
            EvaluatedCSSEntry::Block(_) => {
                return Err(ErrorType::Fontface_BlockNotAllowed);
            }
        }
    }

    Ok(FontFace{
        keyvals: keyvals
    })

}
