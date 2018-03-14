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