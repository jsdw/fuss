# Types

Expressions in FUSS are all one of the following types:

## Booleans

FUSS supports `true` and `false` booleans, which come in handy for making comparisons in `if` expressions.

```
$t: true;
$f: false;
```

## Units

Units are just numbers with an optional suffix. The following are all examples of valid units:

```
$unit: -1;
$unit: 1.2;
$unit: 10%;
$unit: 27px;
$unit: 200em;
$unit: 0.75deg;
```

## Strings

Strings are useful for interpolating arbitrary content into CSS that FUSS otherwise does not properly understand. Strings are surrounded in double quotes. You need to escape any double quotes or backslashes that you'd like to use using a backslash. These are all valid:

```
$str: "hello";
$str: "hello \"world\"";
$str: "back\\slashes\\need\\escaping\\too";
```

All other characters are interpreted literally in a string.

Strings can be interpolated almost anywhere; the following is all valid (noting that selectors and property names require a `${}` to interpolate FUSS expressions into them):

```
$myString: "awesome";
$prefix: "background";
$value: "a-made-up-" + "color-name";

${ $myString }-class {
    ${ $prefix }-color: $value;
}
```

## Colours

Colours are primaily declared in one of the following ways:

```
$col: blue;
$col: rgb(0,0,255);
$col: rgba(0,0,255,0.5);
$col: hsl(240deg,100%,50%);
$col: hsla(240deg,100%,50%, 0.5);
```

All of the colour names supported in CSS are also supported in FUSS.

## Variables

Variables can be declared at the top level of a file, or inside a block. They don't have to be declared before being used, and they are scoped to the file or block in which they are declared (whichever is smaller in scope).

Basic usage:

```
$b: blue;

.hello {
	color: $b;
}
```

This has exactly the same output:

```
.hello {
	color: $b;
}

$b: blue;
```

As does this:

```
.hello {
    $b: blue; // $b is not known about outside of .hello
	color: $b;
}
```

## If expressions

Currently the only branching mechanism in FUSS; `if` expressions allow a different result to be returned based on the result of some condition. The condition evaluates to `false` if it's a 0 valued unit, the literal `false`, an empty string or `undefined`, and `true` otherwise. Usage:

```
$val: 100;
// This evaluates to the string "yes":
$yes: if $val >= 100 then "yes" else "no";
// This evaluates to the string "no":
$no: if $val < 100 then "yes" else "no";
```

## Blocks

Blocks are the core building block of CSS. Blocks optionally have a selector, and inside them we can declare variables, key-value pairs of CSS properties, or nest other blocks. One can also access variables declared inside blocks, making them useful as named arguments to functions.

In their most basic form, blocks allow reuse of common key-value pairs:

```
$block: {
	color: blue;
	padding: 100px;
};

.hello {
	$block;
}
.foo {
	$block;
}
```

Blocks can also have their own selectors:

```
$block: .world {
	color: blue;
	padding: 100px;
};

// outputs a blue, 100px ".hello .world" block:
.hello {
	$block;
}
```

We can declare variables and other blocks inside blocks, so this is equivalent to the above:

```
$block: .hello {
	$col: blue;
	.world {
		$pad: 100px;
		color: $col;
		padding: $pad;
	}
};

$block;
```

We can pluck out the values of variables inside blocks as well:

```
$block:{
	$a: {
		$b: 100px;
	};
	$c: blue;
};

.hello {
	padding: $block.a.b;
	colour: $block.c;
}
```

A neat trick if you want to declare intermediate variables without exposing them outside of a function is to work inside a block and then return only what you want from it. Here, `$a` is 3:

```
$a: ({
	// these internal variables are hidden outside this block:
	$a: 1px;
	$b: 2;
	// we access $res from the block to return it. Parens are
	// required around the block to make parsing simpler:
	$res: $a + $b;
}).res;

.hello {
	padding: $a;
}
```

Passing and returning blocks is particularly useful within functions..

## Functions

A function in FUSS has access to variables defined in the enclosing blocks at the point of its definition, and can also take arguments. Some examples:

```
$fn: ($a) => if $a then $a + 2 else 100px;
$fn: () => true;
$fn: ($a, $b) => $a + $b * 2;
```

Functions can combine with blocks to allow for something like named arguments:

```
$fn: ($b) => {
    padding: $b.padding;
    color: $b.colour;
    border-radius: ${ if $b.border then $b.border else 5px };
    margin: 5px;
};

.hello {
    $fn({ $padding: 5px; $colour: red });
}
```

The above function both takes and returns a block. It allows for an optional $border argument to be provided, defaulting to a 5px border radius if not.

## Undefined

`undefined` is a special value given back on trying to access properties or function arguments that have not been given a value. It cannot however be interpolated into CSS, preventing accidental misuse. `undefined` allows us to create functions which take optional arguments amoung other things.

```
$fn: ($val) => if $val == undefined then 100px else $val;

.hello {
	padding: $fn(); // defaults to 100px.
	margin: $fn(200px); // 200px.
}
```