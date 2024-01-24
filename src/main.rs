use lang::{ast::Chunk, code::Closure, value::{Function, Value}};
use scan::{compiler::{Compilable, Compiler}, interpreter::Interpreter, lexer::Lexer, parser::Parsable, position::Located};
use std::{cell::RefCell, env, error::Error, fs, process, rc::Rc};

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
pub fn compile(text: &str) -> Result<Rc<RefCell<Closure>>, Located<Box<dyn Error>>> {
    let ast = parse(text)?;
    ast.compile(&mut Compiler::default()).map_err(|err| err.map(|err| err.into()))
}
pub fn run(text: &str) -> Result<Option<Value>, Located<Box<dyn Error>>> {
    let closure = compile(text)?;
    dbg!(&closure);
    let function = Rc::new(Function {
        closure,
        upvalues: vec![]
    });
    let mut interpreter = Interpreter::default();
    interpreter.call(&function, None);
    interpreter.run().map_err(|err| err.map(|err| err.into()))
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
        let value = run(&text)
            .map_err(|Located { value: err, pos }| {
                eprintln!(
                    "ERROR {path}:{}:{}: {err}",
                    pos.ln.start + 1,
                    pos.col.start + 1
                );
                process::exit(1);
            })
            .unwrap();
        dbg!(value);
    }
}
