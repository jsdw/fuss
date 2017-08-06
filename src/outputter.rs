use types::*;

/// Take a `NestedSimpleBlock` and convert it into
/// a valid CSS string.
pub fn to_css(block: &Block) -> String {
    let v = flatten("", block);
    print_flattened(&v)
}

struct FlattenedBlock<'a> {
    selector: String,
    inner: Vec<(&'a str, &'a str)>
}

/// Print the flattened blocks out:
fn print_flattened(blocks: &Vec<FlattenedBlock>) -> String {

    let mut out = String::new();
    for block in blocks {

        // css keyvals aren't allowed to not have
        // a selector; for now just ignore any that don't:
        if block.selector.len() == 0 {
            continue;
        }

        out += &block.selector;
        out += " {\n";

        for inner in &block.inner {
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

/// Recurse over Blocks and push plain css to our buffer.
fn flatten<'a>(selector: &str, block_enum: &'a Block) -> Vec<FlattenedBlock<'a>> {

    let mut output = vec![];
    match *block_enum {

        Block::CSSBlock(ref block) => {

            // if block.css.len() == 0 {
            //     return vec![];
            // }

            // let this_selector = merge_selectors(selector, &block.selector);
            // let mut this_block = FlattenedBlock{
            //     selector: this_selector.clone(),
            //     inner: vec![]
            // };

            // for entry in &block.css {
            //     match *entry {
            //         CSSEntry::KeyVal{ ref key, ref val } => {
            //             this_block.inner.push((key,val));
            //         },
            //         CSSEntry::Expr(ref expr) => {

            //             // we've seen another block, so commit our
            //             // current one to the list if it's not empty:
            //             if this_block.inner.len() > 0 {
            //                 output.push(this_block);
            //                 this_block = FlattenedBlock{
            //                     selector: this_selector.clone(),
            //                     inner: vec![]
            //                 }
            //             }

            //             // append all blocks returned to our list:
            //             let mut next_blocks = flatten(&this_block.selector, &boxed_block );
            //             output.append(&mut next_blocks);

            //         }
            //     }
            // }

            // // commit out current block to the list if it's not empty:
            // if this_block.inner.len() > 0 {
            //     output.push(this_block);
            // }

        },

        _ => {
            // ignore anything we don't know about yet
            panic!("Don't know how to output: {:?}", block_enum)
        }

    };
    output

}

/// merge two CSS selectors together. if both are empty, return an
/// empty string. we do our best to trim things.
fn merge_selectors(first: &str, second: &str) -> String {

    let first = first.trim();
    let second = second.trim();

    if first.len() == 0 && second.len() == 0 {
        return String::new();
    }
    if first.len() == 0 {
        return second.to_owned();
    }
    if second.len() == 0 {
        return first.to_owned();
    }

    first.to_owned() + " " + second
}