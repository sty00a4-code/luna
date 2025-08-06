# String Module
All the functions in this module can be self-called on any string value, if that function expects a string as the first argument.

#### `string.lowercase: vector<char>`
An array of all lowercase letters.

#### `string.uppercase: vector<char>`
An array of all uppercase letters.

#### `string.letters: vector<char>`
An array of all alphabetic letters.

#### `string.from(...): string`
Joins all the arguments together into a string.

#### `string.len(s: string): int`
Returns the length of the `s` string.

#### `string.iter(s: string): iterator<char>`
Returns an iterator over the chars in the `s` string.

#### `string.get(s: string, index: int, default: char?): char?`
Returns the char at the `index` of the `s` string. Defaults to the `default` char if given.

#### `string.sub(s: string, i: int, j: int?, default: string?): string?`
Returns the sub-string of `s` from index `i` to `j`. If `j` is not given, it is the length of the string. If the indexes are out of range, `default` will be returned.

#### `string.split(s: string, sep: string): vector<string>`
Returns a vector of the string `s` split by `sep`.

#### `string.split_amount(s: string, n: int, sep: string): vector<string>`
Returns a vector of the string `s` split by `sep`, but only `n` amount of times.

#### `string.split_at(s: string, idx: int): vector<string>`
Returns a vector of the string `s` split at the index `idx`.

#### `string.split_off(s: string, idx: int): string`
Returns a the string `s` from index `idx` to the end of the string.

#### `string.rep(s: string, n: int): string`
Returns a repetition of the `s` string by `n`.

#### `string.rev(s: string): string`
Returns the reverse of `s`.

#### `string.find(s: string, patt: string): string`
Returns the starting position of the `patt` sub-string in `s`.

#### `string.format(s: string, ...): string`
Returns the formatted version of `s` with the rest of the arguments.

- `%s`: next argument as a string
- `%q`: next argument quoted if it is a string
- `%x`: next argument as a hexadecimal integer if it is a integer
- `%%`: just `%`

#### `string.start(s: string, start: string): boolean`
Returns true if the string `s` starts with the substring `start`.

#### `string.end(s: string, end: string): boolean`
Returns true if the string `s` ends with the substring `end`.

#### `string.rmatch(s: string, pattern: string): string|vector<string>?`
Returns the substring(s) in `s` that match the regex pattern `pattern`.