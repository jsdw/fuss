- [Documentation](docs/index.md)
- [Examples](examples)

# FUSS - A Functional CSS Preprocessor.

The goal of FUSS is to improve on the power to weight ratio of existing CSS preprocessors. The syntax aims to be a superset of CSS, so you can write CSS as you would normally, but augment it with FUSS syntax when you'd like the extra functionality.

In FUSS, almost everything is an expression. Functions, variables, and CSS blocks themselves can all be composed with eachother. FUSS looks something like:

```
$other: import("other");

$drop: &.drop {
    border: 3px solid yellow;
    .icon {
        color: yellow;
    }
};

.site-body {

    // apply our other theme here,
    // overriding the title colors:
    $other.theme({
        $title: red;
        $titleBg: black;
    });

    // add drop styling too:
    $drop;

    &.massive .banner, &.huge .banner {
        height: 100px;
    }

}
```

Check out the [examples](examples) folder for more.

## Installation

The easiest way to have a play with FUSS at the moment is to build from source. This is very straightforward:

1. install rust from [the official website](https://www.rust-lang.org).
2. run `cargo install` in this folder to build an optimised binary.

## Note

This is a prototype/work in progress. Many things work great, but there is still much to do before it can cover the vast majority of use cases!

## Contributions

Feedback and pull requests are welcome! Please open an issue prior to doing any serious amount of work so that we can have a chat and make sure we're on the same page :)