# ENV Module

#### `env.set_var(key: string, value)`
Sets the environment variable `key` to the `value` as a string for the currently running process.

#### `env.var(key: string): string?`
Fetches the environment variable `key` from the current process.

#### `env.remove_var(key: string)`
Removes an environment variable `key` from the environment of the currently running process.

#### `env.vars(): object<string>`
Returns an object of strings, for all the environment variables of the current process.

#### `env.current_dir(): string?`
Returns the current working directory.

#### `env.current_exe(): string?`
Returns the full filesystem path of the current running executable.

#### `env.args(): string?`
Returns the arguments that this program was started with (normally passed via the command line).