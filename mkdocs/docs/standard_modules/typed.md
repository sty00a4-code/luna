# Typed Module

#### `typed.type(value): string`
Returns the type of `value` as a string. For objects it looks for the meta-field `__type`, but returns `object` as a default.

#### `typed.raw_type(value): string`
Returns the type of `value` as a string, ignoring the objects meta-field `__type`.

#### `typed.check(value: V, ...: string): V?`
Checks if the `value` is of any of the types in the rest of the arguments.

#### `typed.check_raw(value: V, ...: string): V?`
Checks if the `value` is of any of the types in the rest of the arguments. The meta-field `__type` will not have an effect.

#### `typed.int(value): int?`
Checks if the `value` is of type int.

#### `typed.float(value): float?`
Checks if the `value` is of type float.

#### `typed.bool(value): bool?`
Checks if the `value` is of type bool.

#### `typed.char(value): char?`
Checks if the `value` is of type char.

#### `typed.string(value): string?`
Checks if the `value` is of type string.

#### `typed.vector(value): vector?`
Checks if the `value` is of type vector.

#### `typed.object(value): object?`
Checks if the `value` is of type object.

#### `typed.object_raw(value): object?`
Checks if the `value` is of type object. The meta-field `__type` will not have an effect.

#### `typed.function(value): function?`
Checks if the `value` is of type fn.

#### `typed.numeric(value): int|float?`
Checks if the `value` is of type int or float.

#### `typed.iterable(value): string|vector|object?`
Checks if the `value` is of type string, vector or object.

#### `typed.options(value: V, ...): V?`
Checks if the `value` is equal to any of the values in the rest of the arguments.

#### `typed.some(value: V): V?`
Checks if the `value` is not null. Any usual falsy value except null of course return `true`.