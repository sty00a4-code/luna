# Globals

#### `print(...)`
Prints all the arguments to *stdout* transforming them into strings. 

#### `input(prefix: string): string`
Reads the input from *stdin* until enter has been pressed and returns it as a string. *(the string contents are being trimmed)*

#### `assert(cond)`
If `cond` is a false value, an error will be thrown.

#### `error(msg)`
Throws an error with the message `msg` which is stringified first.

#### `exit(code: int?)`
Exits the program with the exit code `code`. *(default is `0`)*

#### `safe_call(f: fn, ...): { ok?, err: string? }`
Run's the function in safe manner. Errors will not cause the program to crash, but the error will be returned in form of an object as the field `err`. If it runs successfully the field `ok` holds the return value.

#### `range(start: int, end: int?): iterator<int>`
Creates an iterator over the given range defined by `start` and `end`. If `end` is null, `start` will be `0` and `end` will be the `start`

#### `raw_get(o: object, k: string)`
Returns the value with the key `k` in object `o`, without calling the custom meta-function `__get` defined for `o` 

#### `raw_set(o: object, k: string, value)`
Sets the value with the key `k` in object `o` to `value`, without calling the custom meta-function `__set` defined for `o`

#### `iter(iterable: string|vector|object): iterator`
Call the iterator function for the value `iterable`

#### `next(iter: iterator<V>|object): V?`
Calls the `next` function of the `iter` iterator. If `iter` is an object that defined the meta-function `__next`, that function will be called.

#### `type(value): string`
*definition: [`typed.type`](typed.md#typed.type)*

#### `raw_type(value): string`
*definition: [`typed.raw_type`](typed.md#raw_typed.type)*

#### `options(value, ...): bool`
*definition: [`typed.options`](typed.md#typed.options)*

#### `some(value): bool`
*definition: [`typed.some`](typed.md#typed.some)*

#### `str(...): string`
*definition: [`string.from`](type_modules/string_module.md#string.from)*

#### `keys(o: object): iterator<string>`
*definition: [`object.keys`](type_modules/object_module.md#object.keys)*

#### `values(o: object<V>): iterator<V>`
*definition: [`object.values`](type_modules/object_module.md#object.values)*

#### `setmeta(o: object<V>, meta: object?): object<V>`
*definition: [`object.setmeta`](type_modules/object_module.md#object.setmeta)*

#### `getmeta(o: object): object?`
*definition: [`object.getmeta`](type_modules/object_module.md#object.getmeta)*
