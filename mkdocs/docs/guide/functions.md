# Functions
Functions in Luna are first-class values, which means they can be passed around like any other native value type. A function consists of a closure reference and an [UpValue](#upvalues) list. A normal code Chunk is also just a function, that gets executed immediatly after compilation.
```
let fn add(a, b) {
    return a + b
}
print(add(1, 2))
```
This code generates two closures (code instance that can reference other closures), one is the *main* closure, and the other is the `add` function. The *main* closure has a reference to the `add` closure, because it needs to assign it to the local `add` variable as a function referencing that closure (note that this function has no upvalues).

## UpValues
UpValues are an important concept in Luna, as they let functions keep references to values even though their scope is not active anymore. The best example is a counter function.
```
let fn counter() {
    let count = 1
    return fn () {
        count += 1
        return count
    }
}
```
This function called `counter` returns a function that references a closure, but also has to keep a reference to the value saved in `count`, because it uses it to increment it's counting value. The compiler automatically infers that the `count` variable from the outer scope is being referenced here, so it saves a copy to it's reference. Because Luna has a reference-counting garbage collector, the reference will not be invalid after the `counter` function is done executing. If it weren't referenced, the `count` variable would be dropped from memory. As long as the function still exists, this value reference exists too. The reference is also never the same for each call to `counter`.
```
let c1 = counter() # function with a unique reference to `count`
let c2 = counter() # another function with a unique reference to another `count`
```
Here we have to different functions being assigned to `c1` and `c2`. Both functions reference the same closure, but not the same `count` value. This allows multiple counters to exist in this program making the following code work like you would expect:
```
print(c1()) # 1
print(c1()) # 2
print(c1()) # 3
print(c2()) # 1
print(c2()) # 2
print(c2()) # 3
```