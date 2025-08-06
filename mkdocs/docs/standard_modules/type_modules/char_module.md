# Char Module
All the functions in this module can be self-called on any char value, if that function expects a char as the first argument.

#### `char.from(value): char?`
Casts the `value` into a char if it is possible.

#### `char.byte(value: char): int`
Casts the `value` char into it's byte version.

#### `char.is_whitespace(value: char): bool`
Checks if `value` char is a whitespace character.

#### `char.is_alphabetic(value: char): bool`
Checks if `value` char is a alphabetic character.

#### `char.is_alphanumeric(value: char): bool`
Checks if `value` char is a alphanumeric character.

#### `char.is_control(value: char): bool`
Checks if `value` char is a control character.

#### `char.is_digit(value: char): bool`
Checks if `value` char is a digit character.

#### `char.is_graphic(value: char): bool`
Checks if `value` char is a graphic character.

#### `char.is_hex(value: char): bool`
Checks if `value` char is a hexadecimal character.

#### `char.is_lower(value: char): bool`
Checks if `value` char is a lowercase character.

#### `char.is_upper(value: char): bool`
Checks if `value` char is an uppercase character.
