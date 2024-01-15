use std::{env, fs, process};

pub mod position;
pub mod lang;

fn main() {
    let mut args = env::args().skip(1);
    if let Some(path) = args.next() {
        let text = fs::read_to_string(&path).map_err(|err| {
            eprintln!("ERROR: error while reading {path:?}: {err}");
            process::exit(1);
        }).unwrap();
        println!("{text:?}");
    }
}