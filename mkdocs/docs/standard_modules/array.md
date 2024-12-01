# Array Module

#### `array.u8(length: int?, init: vector<int>?): array<u8>`
Creates a new concise array of only `u8` values.

#### `array.u16(length: int?, init: vector<int>?): array<u16>`
Creates a new concise array of only `u16` values.

#### `array.u32(length: int?, init: vector<int>?): array<u32>`
Creates a new concise array of only `u32` values.

#### `array.u64(length: int?, init: vector<int>?): array<u64>`
Creates a new concise array of only `u64` values.

#### `array.u128(length: int?, init: vector<int>?): array<u128>`
Creates a new concise array of only `u128` values.

#### `array.i8(length: int?, init: vector<int>?): array<i8>`
Creates a new concise array of only `i8` values.

#### `array.i16(length: int?, init: vector<int>?): array<i16>`
Creates a new concise array of only `u16` values.

#### `array.i32(length: int?, init: vector<int>?): array<i32>`
Creates a new concise array of only `i32` values.

#### `array.i64(length: int?, init: vector<int>?): array<i64>`
Creates a new concise array of only `i64` values.

#### `array.i128(length: int?, init: vector<int>?): array<i128>`
Creates a new concise array of only `i128` values.

#### `array.f32(length: float?, init: vector<float>?): array<f32>`
Creates a new concise array of only `f32` values.

#### `array.f64(length: float?, init: vector<float>?): array<f64>`
Creates a new concise array of only `f64` values.

#### `array.bool(length: bool?, init: vector<bool>?): array<bool>`
Creates a new concise array of only `bool` values.

#### `array.char(length: char?, init: vector<char>?): array<char>`
Creates a new concise array of only `char` values.

## Array

All the functions in these objects can only be self-called on the array user-objects, if that function expects the array user-objects as the first argument.

#### `<array<T>>.iter(self: array<T>): array-iterator<T>`
Returns an `array-iterator<T>` user-object over the containing values

#### `<array<T>>.get(self: array<T>, index: int, default: T?): T?`
Returns the value in the array at position `index`, otherwise it returns `default`.

#### `<array<T>>.set(self: array<T>, index: int, value: T)`
Sets the value at position `index` to `value`.

#### `<array<T>>.push(self: array<T>, value: T)`
Pushes `value` to the end of the array.

#### `<array<T>>.pop(self: array<T>, index: int?): T?`
Pops the last value off of the array and returns it, or if `index` is given, removes the value from that position and returns it.

#### `<array<T>>.insert(self: array<T>, index: int, value: T)`
Inserts `value` in the array at position `index`.

#### `<array<T>>.swap(self: array<T>, index1: int, index2: int)`
Swaps the two values in the array at position `index1` and `index2`.

#### `<array<T>>.len(self: array<T>): int`
Returns the length of the array.

#### `<array<T>>.contains(self: array<T>, value: T): bool`
Returns true, if `value` is in the array.

#### `<array<T>>.position(self: array<T>, value: T): int?`
Returns the position of `value`, if it even is in the array.

#### `<array<T>>.join(self: array<T>, sep: string): string`
Returns all the values in the array as a string joined by `sep`.

#### `<array<T>>.copy(self: array<T>): array<T>`
Returns a copy of the array.

#### `<array<T>>.clear(self: array<T>)`
Clears the array.

#### Only works on integer arrays

#### `<array<T>>.sort(self: array<T>)`
Sorts the array.

#### `<array<T>>.max(self: array<T>): T?`
Returns the biggest value of the array.

#### `<array<T>>.min(self: array<T>): T?`
Returns the smallest value of the array.

## Array Iterator

All the functions in these objects can only be self-called on the array-iterator user-objects, if that function expects the array-iterator user-objects as the first argument.

#### `<array-iterator<T>>.next(self: array-iterator<T>): T?`
Returns the next value in the array iterator.

#### `<array-iterator<T>>.all(self: array-iterator<T>, f: fn): bool`
Returns true, if all of the values in the iterator return true if passed into `f`.

#### `<array-iterator<T>>.any(self: array-iterator<T>, f: fn): bool`
Returns true, if any of the values in the iterator return true if passed into `f`.

#### `<array-iterator<T>>.collect(self: array-iterator<T>): array<T>`
Returns an array with all the rest of the values of the array iterator in it.
