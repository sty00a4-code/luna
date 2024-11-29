# Object Module

#### `object.len(o: object): int`
Returns the amount of the objects fields.

#### `object.keys(o: object): iterator<string>`
Returns an iterator over all the fields of `o`.

#### `object.values(o: object<V>): iterator<V>`
Returns an iterator over all the values of `o`.

#### `object.setmeta(o: object<V>|fn, meta: object?): object<V>|fn`
Sets the meta-object of `o` to `meta`

#### `object.getmeta(o: object|fn): object?`
Returns the meta-object of `o` if it has any.

#### `object.clear(o: object)`
Clears `o` of it's values.

#### `object.copy(o: object): object`
Copies `o`'s fields into a new object.

#### `object.box(v): box`
Returns the value `v` wrapped around a box. The box holds the inner value to be passed around by reference, without copying the inner value. This allows multiple variables to reference the same value.
To read the inner value write `box.value`.
To change the inner value write `box.value = new_value`.

#### `object.int(v: int): int-box`
Returns the value `v` wrapped around a box that can only hold an integer.

#### `object.float(v: float): float-box`
Returns the value `v` wrapped around a box that can only hold an float.

#### `object.bool(v: boolean): boolean-box`
Returns the value `v` wrapped around a box that can only hold an boolean.

#### `object.char(v: char): char-box`
Returns the value `v` wrapped around a box that can only hold an character.

#### `object.string(v: string): string-box`
Returns the value `v` wrapped around a box that can only hold an string.

#### `object.set(...): set`
Returns a set with the given values.

## Box
All the functions in this module can only be self-called on the box user-object, if that function expects a box user-object as the first argument.

#### `<box>.clone(self: box): box`
Returns a clone of `self`.

### Meta

#### `<box>.__set(self: box, key: "value", value)`
Sets the inner value of the box to `value`.

#### `<box>.__str(self: box): string`
Returns the string representation of the box.

## Set
All the functions in this module can only be self-called on the set user-object, if that function expects a set user-object as the first argument.

#### `<set>.clone(self: set): set`
Returns a clone of `self`.

### Meta

#### `<set>.__set(self: set, key, value: bool)`
Adds/Removes the value `key` from the set.

#### `<set>.__get(self: set, key: K): K?`
Returns the value `key` if it's in the set.

#### `<set>.__or(self: set, other: set): set`
Returns the union of `self` with `other`.

#### `<set>.__and(self: set, other: set): set`
Returns the intersection of `self` with `other`.

#### `<set>.__sub(self: set, other: set): set`
Returns `self` with `other` removed.

#### `<set>.__lt(self: set, other: set): boolean`
Returns true if `self` is a subset of `other`.

#### `<set>.__gt(self: set, other: set): boolean`
Returns true if `self` is a supperset of `other`.

#### `<set>.__le(self: set, other: set): boolean`
Returns true if `self` is a subset of `other` or the same.

#### `<set>.__ge(self: set, other: set): boolean`
Returns true if `self` is a supperset of `other` or the same.

#### `<set>.__str(self: set): string`
Returns the string representation of the set.