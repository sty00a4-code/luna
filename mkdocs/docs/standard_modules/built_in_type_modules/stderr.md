# Stderr
All the functions in this module can only be self-called on the stderr user-object, if that function expects a stderr user-object as the first argument.

#### `<stderr>.write(self: stderr, content: string)`
Writes to the stderr buffer.

#### `<stderr>.flush(self: stderr)`
Flushes to the stderr buffer.
