# Vector Module
All the functions in this module can be self-called on any vector value, if that function expects a vector as the first argument.

#### `vector.iter(v: vector<T>): iterator<T>`
Returns an iterator over the elements of `v`.

#### `vector.len(v: vector): int`
Returns the length of `v`.

#### `vector.get(v: vector, index: int, default?)`
Returns the value of `v` at index `index`. If the index is out of range, `default` is returned

#### `vector.contains(v: vector, value): bool`
Checks if the `value` is contained in `v`.

#### `vector.position(value): int?`
Returns the index of the element in the vector that equal `value`, if there is any.

#### `vector.push(v: vector, value)`
Pushes the `value` onto `v`.

#### `vector.pop(v: vector<T>, index: int?): T?`
Pops value at `index` off of `v` and returns it. If `index` is not given, the last value in `v` will be popped.

#### `vector.join(v: vector, sep: string): string`
Returns a string of all the values in `v` as strings seperated by `sep`.

#### `vector.swap(v: vector, i1: int, i1: int)`
Swaps the index `i1` with index `i2` of `v` if they are in bounds.

#### `vector.copy(v: vector<T>): vector<T>`
Returns a shallow copy of `v`.

#### `vector.clear(v: vector)`
Clears `v` of it's values.

## Iterator
All the functions in this module can only be self-called on the iterator user-object, if that function expects an iterator user-object as the first argument.

#### `<iterator>.next(iter: iterator<V>): V?`
Returns the next value in the iteration if any are left.

#### `<iterator>.any(iter: iterator<V>, cond: fn(v: V)): boolean`
Returns true if any value of `iter` makes the `cond` function return a truthy value.

#### `<iterator>.all(iter: iterator<V>, cond: fn(v: V)): boolean`
Returns true if all values of `iter` make the `cond` function return a truthy value.

#### `<iterator>.collect(iter: iterator<V>): vector<V>`
Returns a vector of all the values left in the `iter` iterator.
