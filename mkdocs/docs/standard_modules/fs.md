# FS Module

#### `fs.open(path: string, mode: "w"|"r"|"a"): file?`
Opens a file at `path` with the options specified `mode`, if it exists.

#### `fs.list(path: string): vector<string>`
Returns a vector over the entries within a directory.

#### `fs.type(path: string): "dir"|"file"|null`
Returns the type of the `path`.