use lang::ast::Chunk;
use scan::{lexer::Lexer, parser::Parsable, position::Located};
use std::{env, error::Error, fs, process};

pub mod lang;
pub mod scan;

pub fn parse(text: &str) -> Result<Located<Chunk>, Located<Box<dyn Error>>> {
    Chunk::parse(
        &mut Lexer::from(text)
            .lex()
            .map_err(|err| err.map(|err| err.into()))?
            .into_iter()
            .peekable(),
    )
    .map_err(|err| err.map(|err| err.into()))
}

fn main() {
    let mut args = env::args().skip(1);
    if let Some(path) = args.next() {
        let text = fs::read_to_string(&path)
            .map_err(|err| {
                eprintln!("ERROR: error while reading {path:?}: {err}");
                process::exit(1);
            })
            .unwrap();
        let ast = parse(&text)
            .map_err(|Located { value: err, pos }| {
                eprintln!(
                    "ERROR {path}:{}:{}: {err}",
                    pos.ln.start + 1,
                    pos.col.start + 1
                );
                process::exit(1);
            })
            .unwrap();
        dbg!(ast);
    }
}
