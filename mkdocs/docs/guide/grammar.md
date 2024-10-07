# Grammar
The grammar of Luna is very closue to modern languages like JavaScript and Rust

## Atomic Tokens
The following is a list of atomic tokens and what their regex is:

| name   | regex                            |
| ------ | -------------------------------- |
| null   | `/null/`                         |
| ident  | `/[a-zA-Z_]([a-zA-Z_0-9]*)/`     |
| int    | `/[0-9]+/`                       |
| float  | `/[0-9]+\./`, `/[0-9]+\.[0-9]+/` |
| bool   | `/true/`, `/false/`              |
| char   | `/'\.'/`                         |
| string | `/"[^"]*"/`                      |

## Keywords
The following is a list of reserved keywords:

| keyword    | description                                    |
| ---------- | ---------------------------------------------- |
| `let`      | local variable definitions                     |
| `fn`       | function definitions                           |
| `if`       | conditional branches                           |
| `else`     | else conditional branches of if statements     |
| `match`    | match statement                                |
| `while`    | while loops                                    |
| `for`      | for loops                                      |
| `in`       | iterate over something in a for loop statement |
| `return`   | return from a call                             |
| `break`    | break out of a loop                            |
| `continue` | skip to next iteration of a loop               |
| `null`     | represents the null value                      |
| `true`     | represents the boolean value `true`            |
| `false`    | represents the boolean value `false`           |

## Symbols
The following is a list of reserved keywords:

| symbol | name              |
| ------ | ----------------- |
| `=`    | equal             |
| `,`    | comma             |
| `.`    | dot               |
| `:`    | colon             |
| `!`    | exclamation       |
| `(`    | paran left        |
| `)`    | paran right       |
| `[`    | bracket left      |
| `]`    | bracket right     |
| `{`    | brace left        |
| `}`    | brace right       |
| `+`    | plus              |
| `-`    | minus             |
| `*`    | star              |
| `/`    | slash             |
| `%`    | percent           |
| `^`    | exponent          |
| `+=`   | plus equal        |
| `-=`   | minus equal       |
| `*=`   | star equal        |
| `/=`   | slash equal       |
| `%=`   | percent equal     |
| `^=`   | exponent equal    |
| `==`   | equal equal       |
| `!=`   | exclamation equal |
| `<`    | less              |
| `>`    | greater           |
| `<=`   | less equal        |
| `>=`   | greater equal     |
| `&`    | ampersand         |
| `      | `                 | pipe |


## Syntax Sugar
Luna has the same design philosophy as Lua.
What that means is that certain syntax is only another way to write the same thing:

```
fn add(a, b) {
    return a + b
}
```
translates to
```
add = fn(a, b) {
    return a + b
}
```
