# Expressions

An expression has a lot of variants like binary or unary expressions, calls or just be an [Atom](#atom)

## Atom
An Atom is the smallest possible expression, just being one value or a [Path](#path).

### Values
All of the tokens that are atomic like `null`, `int`, `float`, etc. are valid Atoms.

Examples: `1`, `null`, `true`, `"hello there"`

### Sub-Expression
Of course an Atom can also be another [Expression](#expression)

Examples: `(a + b)`, `((a + b) * c)`, `(1)`

### Vector
A list of [Expressions](#expression)

Examples: `[1, 2, 3]`, `[a + b, "joe", false]`

### Object
A list of identifier and [Expression](#expression) pairs

Examples: `{ name = "joe", age = 25 }`

### If-Expression
A conditional expression branch with required if and else-cases.

Examples: `if age > 18 "of age" else "under age"`

### Function Definition
This is used to define functions with parameters as an expression.

Examples: `fn (a, b) { return a + b }`

## Path
A path is either an identifier or a field/index into another path

Examples: `a`, `a.b`, `a.b.c`, `a[1]`