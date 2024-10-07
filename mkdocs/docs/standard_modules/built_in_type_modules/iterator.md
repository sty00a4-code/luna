# Iterator
All the functions in this module can only be self-called on the iterator user-object, if that function expects an iterator user-object as the first argument.

#### `<iterator>.next(iter: iterator<V>): V?`
Returns the next value in the iteration if any are left.

#### `<iterator>.any(iter: iterator<V>, cond: fn(v: V)): boolean`
Returns true if any value of `iter` makes the `cond` function return a truthy value.

#### `<iterator>.all(iter: iterator<V>, cond: fn(v: V)): boolean`
Returns true if all values of `iter` make the `cond` function return a truthy value.

#### `<iterator>.collect(iter: iterator<V>): vector<V>`
Returns a vector of all the values left in the `iter` iterator.
