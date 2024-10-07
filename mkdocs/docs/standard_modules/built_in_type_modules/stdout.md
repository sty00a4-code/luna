# Stdout
All the functions in this module can only be self-called on the stdout user-object, if that function expects a stdout user-object as the first argument.

#### `<stdout>.write(self: stdout, content: string)`
Writes to the stdout buffer.

#### `<stdout>.flush(self: stdout)`
Flushes to the stdout buffer.
