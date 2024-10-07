# Int Module
All the functions in this module can be self-called on any integer value, if that function expects an integer as the first argument.

#### `int.from(value): int?`
Casts the `value` into an integer if it is possible.

#### `int.from_bin(bin: string): int?`
Parses the `bin` string, expected to be a binary letters, to an integer if it is possible.

#### `int.from_hex(hex: string): int?`
Parses the `hex` string, expected to be a hexadecimal letters, to an integer if it is possible.

#### `int.bytes(value: int): vector<int>`
Converts the `value` integer to an array of bytes
