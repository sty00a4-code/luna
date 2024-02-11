use lang::{
    ast::Chunk, code::Closure, tokens::Token, value::{Function, Value}
};
use luna_impl::{
    compiler::{Compilable, Compiler},
    interpreter::Interpreter,
    lexer::Lexer,
    parser::Parsable,
    position::Located,
};
use std::{
    cell::RefCell, collections::{HashMap, HashSet}, env::{self, Args}, error::Error, fmt::Display, fs, process, rc::Rc
};

pub mod lang;
pub mod luna_impl;
#[cfg(test)]
pub mod tests;

pub fn lex(text: &str, args: &LunaArgs) -> Result<Vec<Located<Token>>, Located<Box<dyn Error>>> {
    let tokens = Lexer::from(text)
        .lex()
        .map_err(|err| err.map(|err| err.into()))?;
    if args.tokens {
        println!("TOKENS:");
        for token in tokens.iter() {
            println!("{token:?}")
        }
    }
    Ok(tokens)
}
pub fn parse(text: &str, args: &LunaArgs) -> Result<Located<Chunk>, Located<Box<dyn Error>>> {
    let ast = Chunk::parse(
        &mut lex(text, args)?
            .into_iter()
            .peekable(),
    )
    .map_err(|err| err.map(|err| err.into()))?;
    if args.ast {
        println!("AST:");
        dbg!(&ast);
    }
    Ok(ast)
}
pub fn compile(text: &str, args: &LunaArgs) -> Result<Rc<RefCell<Closure>>, Located<Box<dyn Error>>> {
    let ast = parse(text, args)?;
    let closure = ast.compile(&mut Compiler::default())?;
    if args.code {
        println!("CODE:");
        println!("{}", closure.borrow());
    }
    Ok(closure)
}
pub fn run(text: &str, args: &LunaArgs) -> Result<Option<Value>, Located<Box<dyn Error>>> {
    let closure = compile(text, args)?;
    let function = Rc::new(Function {
        closure,
        upvalues: vec![],
    });
    let mut interpreter = Interpreter::default().with_global_path(env::var("LUNA_PATH").ok());
    interpreter.call(&function, vec![], None);
    interpreter.run().map_err(|err| err.map(|err| err.into()))
}

fn main() {
    let args = LunaArgs::try_from(env::args()).map_err(|err| {
        eprintln!("{err}");
        process::exit(1);
    }).unwrap();
    let text = fs::read_to_string(&args.path)
        .map_err(|err| {
            eprintln!("ERROR: error while reading {:?}: {err}", args.path);
            process::exit(1);
        })
        .unwrap();
    let value = run(&text, &args)
        .map_err(|Located { value: err, pos }| {
            eprintln!(
                "ERROR {}:{}:{}: {err}",
                args.path,
                pos.ln.start + 1,
                pos.col.start + 1
            );
            process::exit(1);
        })
        .unwrap();
    if let Some(value) = value {
        println!("{value}");
    }
}

#[derive(Debug, Clone, Default)]
pub struct LunaArgs {
    path: String,

    tokens: bool,
    ast: bool,
    code: bool
}
#[derive(Debug, Clone, PartialEq)]
pub struct LunaArgsError;
pub const USAGE: &str = r#"USAGE:
    luna <input.luna> [OPTIONS]
    OPTIONS:
        -t  display lexer tokens
        -a  display parser ast
        -c  display compiler code
"#;
impl Display for LunaArgsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", USAGE)
    }
}
impl TryFrom<Args> for LunaArgs {
    type Error = LunaArgsError;
    fn try_from(args: Args) -> Result<Self, Self::Error> {
        let mut args = args.skip(1);
        let mut singles: Vec<String> = vec![];
        let mut attributes: HashMap<String, String> = HashMap::new();
        let mut flags: HashSet<String> = HashSet::new();
        while let Some(arg) = args.next() {
            if arg.starts_with("--") {
                if let Some(flag) = arg.get(2..) {
                    if let Some(value) = args.next() {
                        attributes.insert(flag.to_string(), value);
                    }
                } else {
                    singles.extend(args);
                    break;
                }
            } else if arg.starts_with('-') {
                if let Some(flag) = arg.get(1..) {
                    flags.insert(flag.to_string());
                } else {
                    singles.extend(args);
                    break;
                }
            } else {
                singles.push(arg);
            }
        }
        if flags.contains("help") || flags.contains("h") {
            return Err(LunaArgsError)
        }
        Ok(Self {
            path: if !singles.is_empty() {
                singles.remove(0)
            } else {
                return Err(LunaArgsError)
            },
            tokens: flags.contains("tokens") || flags.contains("t"),
            ast: flags.contains("ast") || flags.contains("a"),
            code: flags.contains("code") || flags.contains("c"),
        })
    }
}
