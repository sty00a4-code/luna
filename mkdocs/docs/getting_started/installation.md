# Installation

To install Luna you have to have `cargo` installed on your system as well as `rustc` and build the project yourself.

## Linux

1. Clone the repository `gh repo clone sty00a4-code/luna` or download the source code
2. In the new folder, build the code with `cargo bundle --release` (gives you the path `target/release/bundle/deb/luna_{VERSION}_{CPU}.deb`)
3. `sudo dpkg -i target/release/bundle/deb/luna_{VERSION}_{CPU}.deb`

## Windows

not yet supported due to `cargo bundle` not working properly on windows systems.
it's best to use the Ubuntu [subsystem](https://ubuntu.com/desktop/wsl) for Windows and install it on there with the [Linux](#linux) instructions.