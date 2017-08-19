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
