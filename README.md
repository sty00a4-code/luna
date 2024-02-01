# Luna

A light weight scripting language for quick and easy coding

# TODO

- [x] easier self call on vectors and strings ~~, requires new bytecode `SelfCall`~~
- [x] `require(path)`
- [x] `string` module
    - [x] `:rep(n)`
    - [x] `:rev()`
    - [x] `:find(patt)`
    - [x] `:format(...values)`
- [x] `vector` module
    - [x] `:insert(idx, value)`
    - [x] `:join(sep)`
    - [x] `:swap(idx1, idx2)`
    - [ ] ~~`:sort()`~~
- [x] `math` module
    - [x] `.random()`
    - [x] `.deg(rad)`
    - [x] `.rad(deg)`
- [ ] `os` module
    - [ ] `.clock()`
    - [ ] `.date(format, time)`
    - [ ] `.time()`
    - [ ] `.exec(command)`
    - [ ] `.exit(code)`
    - [ ] `.get_var(name)`
    - [ ] `.set_var(name, value)`