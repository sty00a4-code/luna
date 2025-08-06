# Statements
A Luna program is basically a list of statements, called a **Chunk**.

## Chunk
A list of statements with no seperator

## Block
Just a list of statements like a [Chunk](#chunk), but it requires `{` at the start and `}` at the end.

## Statement
A statement has a lot of variants, like let-bindings, function definitions, etc. But it also can as well just be a [Block](#block)

### Let Binding
This is what you define local variables with. The assignees can be simple identifiers or object/vector destructurings.

Examples: `let a = 1`, `let { name, age } = person`, `let [first, second, third] = numbers`

### Let-Else Binding
Like a [Let Binding](#let-binding), except if the assigned value is `null`, the else block will be executed

```
let name = person.name else {
    print("no name :(")
    return null
}
```

### Assign
This is how you assign new values to existing variables. The assignees can be simple identifiers or a field/index into an object/vector

Examples: `a = 2`

### Assign Operation
Written the same way as [Assign](#assign) expect with any of the following assign operators, only one assignee and only one expression:

| assign operators |
| ---------------- |
| `+=`             |
| `-=`             |
| `*=`             |
| `/=`             |
| `^=`             |
| `%=`             |


Examples: `a += 1`, `b *= 2`

### Call
This is to call functions with arguments. The callee can either be a simple identifier or a field/index of an object/vector.

Examples: `print("Hello, World!")`, `self.add(1)`

### Self Call
Just like a [Call](#call) only written with a `:` followed by an identifier. This calls the function with the first argument being the value before the `:`. This is used to call objects with themselfs for object-oriented design patterns.

Examples: `vector:push(1)`, `person:greet()`

### Function Definition
This is used to **globally** define functions with parameters.

Examples:
```
fn add(a, b) {
    return a + b
}
```

### Let-Function Definition
This is used to **locally** define functions with parameters in the current scope.

Examples:
```
let fn add(a, b) {
    return a + b
}
```

### If-Branch
Classical if-branch like any other. Has an optional else-case which can either be a block or another if-branch.

Examples:
```
if cond {
    print("yay")
}
```
```
if cond {
    print("yay")
} else {
    print("nay")
}
```
```
if cond {
    print("yay")
} else if cond2 {
    print("semi-yay")
}
```

### If-Branch
Assignes the right side of the equal to the parameter on the left and only enters the branch, if the value is not `null`.

Examples:
```
if let name = person.name {
    print(name)
}
```
```
if let name = person.name {
    print(name)
} else {
    print("anonymous")
}
```
```
if let name = person.name {
    print(name)
} else if let nickname = person.nickname {
    print(nickname)
}
```

### Match
Classical match-case statement to check over multiple variants of one value.

Examples:
```
match cmd {
    "exit" => { os.exit() }
    cmd if cmd:len() > 0 => {
        print("invalid command: %s":format(cmd))
    }
    cmd => {}
}
```

### While-Loop
Classical while-loop like any other. body has to be a block

Examples:
```
while a < 10 {
    a += 1
}
```

### While-Let-Loop
Assigns the right side of the equals to parameter on the left side. If it is `null`, the loop is exited. 

Examples:
```
while let n = range:next() {
    print(n)
}
```

### For-Loop
Python-like for-loop that requires an iterable to be right of the `in` keyword. Only one variable can be given for each iteration. An iterable can either be an user-object with a `next` method, an object with the meta variable `__next` which is a function, or just a function. 

Examples:
```
for n in [1, 2, 3]:iter() {
    print(n)
}
```
```
for key in keys(map) {
    print(key, map[key])
}
```

### Return
This is used to return a value to the caller and end execution of the current function. 

Examples: `return a + b`, `return null`

### Break
This is used to break out of any loop.

Examples:
```
while true {
    if a >= 10 {
        break
    }
    a += 1
}
```

### Continue
This is used to continue to the next iteration of the loop.

Examples:
```
for n in numbers:iter() {
    if n == 5 {
        continue
    }
    print(n)
}
```