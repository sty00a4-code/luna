# Luna

A light weight scripting language for quick and easy coding

# TODO

- [ ] easier self call on vectors and strings, requires new bytecode `SelfCall`
- [x] `require(path)`
- [ ] `string` module
    - [ ] `:rep(n)`
    - [ ] `:rev()`
    - [ ] `:find(patt)`
    - [ ] `:format(...values)`
- [ ] `vector` module
    - [ ] `:insert(idx, value)`
    - [ ] `:join(sep)`
    - [ ] `:swap(idx1, idx2)`
    - [ ] `:sort()`
- [ ] `math` module
    - [ ] `.random_seed(seed)`
    - [ ] `.random()`
    - [ ] `.deg(rad)`
    - [ ] `.rad(deg)`
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