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
- [ ] `vector` module
    - [ ] `:insert(idx, value)`
    - [ ] `:join(sep)`
    - [ ] `:swap(idx1, idx2)`
    - [ ] `:sort()`
- [x] `math` module
    - [x] `.random()`
    - [x] `.deg(rad)`
    - [x] `.rad(deg)`
- [ ] `io` module
    - [ ] `.remove(path)`
    - [ ] `.rename(old_path, new_path)`
    - [ ] `.list(path)`
- [ ] `os` module
    - [ ] `.clock()`
    - [ ] `.date(format, time)`
    - [ ] `.time()`
    - [ ] `.exec(command)`
    - [ ] `.exit(code)`
    - [ ] `.get_var(name)`
    - [ ] `.set_var(name, value)`