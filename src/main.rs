use std::{env, fs, process};

use scan::{lexer::Lexer, parser::Parsable, position::Located};
use lang::ast::Chunk;

pub mod lang;
pub mod scan;

fn main() {
    let mut args = env::args().skip(1);
    if let Some(path) = args.next() {
        let text = fs::read_to_string(&path)
            .map_err(|err| {
                eprintln!("ERROR: error while reading {path:?}: {err}");
                process::exit(1);
            })
            .unwrap();
        let tokens = Lexer::from(text.as_str())
            .lex()
            .map_err(|Located { value: err, pos }| {
                eprintln!(
                    "ERROR {path}:{}:{}: {err}",
                    pos.ln.start + 1,
                    pos.col.start + 1
                );
                process::exit(1);
            })
            .unwrap();
        let ast = Chunk::parse(&mut tokens.into_iter().peekable()).map_err(|Located { value: err, pos }| {
            eprintln!(
                "ERROR {path}:{}:{}: {err}",
                pos.ln.start + 1,
                pos.col.start + 1
            );
            process::exit(1);
        }).unwrap();
        dbg!(ast);
    }
}
