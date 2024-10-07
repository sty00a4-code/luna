# Variables
Variables in Luna are scoped.
```
let a = 1
{
    let b = 2
    print(a)
    print(b)
}
print(a)
```
Here we created two variables `a` and `b`. `a` is scoped to the entire chunk of code, but `b` on the other hand is only scoped to the two curly braces. This is because we are defining a block with `{` and `}` which has it's own scope that ends at the `}`.

## Parameters
Function parameters are just like local variables, just that they are bound differently.
```
fn add(a, b) {
    let result = a + b
    return result
}
```
When calling this function with arguments as follows, `1` and `2` will be assigned to `a` and `b` for that call:
```
add(1, 2)
```

## Globals
Variables don't have to be assigned locally to a scope though. The other way to create variables is globally by just using an assignment statement with no `let` in front of it.
```
VAR = "this is a global variables"
```
No matter where, after this statement has been executed, does the interpreter not have access to that value through that name.