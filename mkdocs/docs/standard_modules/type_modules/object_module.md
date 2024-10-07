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
