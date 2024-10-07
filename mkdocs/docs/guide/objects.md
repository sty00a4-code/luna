# Objects
Objects are an important part of Luna. They are a map of string-value pairs, that can be read and assigned freely. An object value is only a reference to the actual object, which means passing it around means passing around the reference. Any function that has a reference to an object can modify it's contents.

## Meta Objects
Every object can have an optional reference to another object, being it's meta object. Those objects are just like normal objects, except they can define custom behaviour for the object that references it as an meta object.

```
let person = { name = "joe", age = 25 }
setmeta(person, {})
```
Now the object referenced by `person` got an empty meta object. This meta object does not have any fields that could influence the `person` object.

## Meta Fields
There are a few meta fields predefined in Luna.

| field                                         | effect                                                                                                                                         |
| --------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| `__name: string`                              | when the object is casted to a string, this string will be displayed instead of the default `object`                                           |
| `__type: string`                              | when the object's type is trying to be determined, this string will be returned                                                                |
| `__str: fn(self: object): string`             | when the object is trying to be casted to a string, this function will be called with the object as the first argument                         |
| `__call: fn(self: object, ...)`               | when the object is trying to be called, this function will be called                                                                           |
| `__get: fn(self: object, key: string)`        | when trying to get a field of the object, this function will be called                                                                         |
| `__set: fn(self: object, key: string, value)` | when trying to set a field of the object, this function will be called                                                                         |
| `__next: fn(self: object)`                    | when trying get the next iteration in a for-loop, this function will be called and it's return value will be assign to the for-loop's variable |

## User-Objects
User-objects are externally defined structures in Luna's mother language, Rust. They allow for interoperational functionality between Luna and Rust. This is usefull for file handling for example.
```
let file = fs.open("test.txt", "r")
if some(file) {
    print(file)
    print(file:read())
}
```
output:
```
file:0x631349d52af8 
test test!
```
This code opens a file with the name `file.txt` in **r**ead mode and saves the `file` user-object in the local `file`. if the file was found, then the program first prints the file object and then the content of it with the `read` function.

