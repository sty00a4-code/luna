# FS Module

#### `fs.open(path: string, mode: "w"|"r"|"a"): file?`
Opens a file at `path` with the options specified `mode`, if it exists.

#### `fs.list(path: string): vector<string>`
Returns a vector over the entries within a directory.

#### `fs.type(path: string): "dir"|"file"|null`
Returns the type of the `path`.

## File
All the functions in this module can only be self-called on the file user-object, if that function expects a file user-object as the first argument.

#### `<file>.write(self: file, content: string)`
Writes to the file.

#### `<file>.read(self: file): string`
Returns the content of the file.
