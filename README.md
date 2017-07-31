# FUSS - A Functional CSS Preprocessor.

This is a WIP for a CSS preprocessor that might allow you to write code a bit like...

```
.body {

    $hello: 2;
    $another: ($a, $b) => $a + $b / 2 - 1; /* need to handle infix operators and brackets */
    $returnBlock: ($a) => { $var: $a*2; $another: $a+4 };
    $conditionFn: ($a) => if $a >= 4 then 2px else 4px;
    $child: .another .thing {

        $childA: $hello * 2px;
        $cycleA: $cycle;

        color: blue;
        backgorund-color: red;
        border-width: $another(2px,2px);
        height: $returnBlock(100px).another; //dot method - var $ can be elided

    };

    $cycle: $child.cycleA; // can get into loops like this; need to handle.
    $anotherApplied: $another(2px, 3px);
    $anotherPartial: ($b) => $another(2px, $b);

    $noSelectorNeeded: {

        $size: 50%;

        width: $size;
        display: block;
    };

    $noSelectorNeeded; //plonk the block of css here in .body

    border: ${ $noSelectorNeeded.size / 2 } solid black; //interpolate stuff into property string

    $importedLark: $import("some/other/file"); // access imported stuff through variable.

    $import("other/lark"); // plonk imported stuff into .body.

}
```

...to produce CSS. It aims to improve upon preprocessors like SCSS by offering a flexible, functional style of writing that avoids bad practises like the implicit presense of global variables without any sort of explicit importing of them.

Still in progress and details subject to change!

# Todo:

- [ ] make error messages nicer
- [ ] support colours as first class things
- [x] allow eliding of opening and closing `{`/`}` in new file (just pretend we're inside a block when we start parsing)
- [ ] cache imported files
- [ ] prevent import loops
- [x] no more `${}` in blocks (only in css key/val). Support `$var`, `$var.a`, `$var()`. May help avoid conflicts with css selectors, which only allow `${}`
- [ ] reduce reliance on semicolons. With the above, I suspect we won't need them for css block variable declarations or mixins, just for css values (allow them though)
- [ ] support "//" comments (this might need a change in pest)