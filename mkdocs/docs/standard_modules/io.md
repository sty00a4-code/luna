# IO Module

#### `io.write(s: string)`
Writes `s` to *stdout*.

#### `io.flush()`
Flushes *stdout*.

#### `io.stdin(): stdin`
Returns the *stdin* user-object.

#### `io.stdout(): stdout`
Returns the *stdout* user-object.

#### `io.stderr(): stderr`
Returns the *stderr* user-object.

## Stdin
All the functions in this module can only be self-called on the stdin user-object, if that function expects a stdin user-object as the first argument.

#### `<stdin>.read(self: stdin): string?`
Returns the stdin buffer.

## Stdout
All the functions in this module can only be self-called on the stdout user-object, if that function expects a stdout user-object as the first argument.

#### `<stdout>.write(self: stdout, content: string)`
Writes to the stdout buffer.

#### `<stdout>.flush(self: stdout)`
Flushes to the stdout buffer.

## Stderr
All the functions in this module can only be self-called on the stderr user-object, if that function expects a stderr user-object as the first argument.

#### `<stderr>.write(self: stderr, content: string)`
Writes to the stderr buffer.

#### `<stderr>.flush(self: stderr)`
Flushes to the stderr buffer.
