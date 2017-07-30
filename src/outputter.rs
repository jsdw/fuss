use types::*;

/// Take a `NestedSimpleBlock` and convert it into
/// a valid CSS string.
pub fn to_css(block: &NestedSimpleBlock) -> String {
    let mut s = String::new();
    flatten(&mut s, "", block);
    s
}

/// Recurse over NestedSimpleBlocks and push plain css to our buffer.
fn flatten(buf: &mut String, selector: &str, block: &NestedSimpleBlock) {

    if block.css.len() == 0 {
        return;
    }

    let mut needs_opening = true;
    let mut needs_closing = false;
    for entry in &block.css {
        match *entry {
            NestedCSSEntry::KeyVal{ ref key, ref val } => {
                if needs_opening {
                    needs_opening = false;
                    *buf += &merge_selectors(selector, &block.selector);
                    *buf += " {\n";
                }
                *buf += "\t";
                *buf += key;
                *buf += ": ";
                *buf += val;
                *buf += ";\n";
                needs_closing = true;
            },
            NestedCSSEntry::Block(ref boxed_block) => {
                if needs_closing {
                    *buf += "}\n";
                }
                flatten( buf, &merge_selectors(selector, &block.selector), &boxed_block );
                needs_opening = true;
                needs_closing = false;
            }
        }
    }
    if needs_closing {
        *buf += "}\n";
    }

}

fn merge_selectors(first: &str, second: &str) -> String {
    first.to_owned() + second
}