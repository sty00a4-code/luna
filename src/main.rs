pub extern crate luna_lib;

use luna_lib::{
    lang::{
        ast::Chunk,
        code::Closure,
        tokens::Token,
        value::{Function, Value},
    },
    luna_impl::{compiler::CompilerFrame, position::PathLocated},
};
use luna_lib::{
    lang::{ast::Expression, code::ByteCode},
    luna_impl::{
        compiler::{Compilable, Compiler},
        interpreter::Interpreter,
        lexer::Lexer,
        parser::Parsable,
        position::{Located, Position},
    },
    parse_str,
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    env::{self, Args},
    error::Error,
    fmt::Display,
    fs,
    io::{self, Write},
    process,
    rc::Rc,
};

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
    let ast = Chunk::parse(&mut lex(text, args)?.into_iter().peekable())
        .map_err(|err| err.map(|err| err.into()))?;
    if args.ast {
        println!("AST:");
        dbg!(&ast);
    }
    Ok(ast)
}
pub fn compile(
    text: &str,
    args: &LunaArgs,
) -> Result<Rc<RefCell<Closure>>, Located<Box<dyn Error>>> {
    let ast = parse(text, args)?;
    let closure = ast.compile(&mut Compiler {
        path: args.path.as_ref().cloned(),
        ..Default::default()
    })?;
    if args.code {
        println!("CODE:");
        println!("{}", closure.borrow());
    }
    Ok(closure)
}
pub fn run(text: &str, args: &LunaArgs) -> Result<Option<Value>, PathLocated<Box<dyn Error>>> {
    let closure = compile(text, args).map_err(|err| {
        err.with_path(args.path.as_ref().cloned().unwrap_or("<input>".to_string()))
    })?;
    let function = Rc::new(Function {
        closure,
        upvalues: vec![],
    });
    let mut interpreter = Interpreter::default().with_global_path(env::var("LUNA_PATH").ok());
    interpreter.call(&function, vec![], None);
    interpreter.run().map_err(|err| {
        err.map(|err| err.into())
            .with_path(interpreter.path().unwrap_or("<input>".to_string()))
    })
}

fn main() {
    let args = LunaArgs::try_from(env::args())
        .map_err(|err| {
            eprintln!("{err}");
            process::exit(1);
        })
        .unwrap();
    if let Some(path) = &args.path {
        let text = fs::read_to_string(path)
            .map_err(|err| {
                eprintln!("ERROR: error while reading {:?}: {err}", args.path);
                process::exit(1);
            })
            .unwrap();
        let value = run(&text, &args)
            .map_err(
                |PathLocated {
                     value: err,
                     path,
                     pos,
                 }| {
                    eprintln!(
                        "ERROR {path}:{}:{}: {err}",
                        pos.ln.start + 1,
                        pos.col.start + 1
                    );
                },
            )
            .unwrap();
        if let Some(value) = value {
            println!("{value}");
        }
    } else {
        let mut interpreter = Interpreter::default().with_global_path(env::var("LUNA_PATH").ok());
        loop {
            let mut input = String::new();
            print!("> ");
            let _ = io::stdout().flush();
            io::stdin()
                .read_line(&mut input)
                .map_err(|err| {
                    eprintln!("ERROR: error while reading from input: {err}");
                    process::exit(1);
                })
                .unwrap();
            let closure = match parse_str::<Expression>(&input) {
                Ok(expr) => {
                    let mut compiler = Compiler::default();
                    compiler.push_frame(CompilerFrame::default());
                    let src = match expr.compile(&mut compiler) {
                        Ok(src) => src,
                        Err(Located { value: err, pos }) => {
                            eprintln!("ERROR {}:{}: {err:?}", pos.ln.start + 1, pos.col.start + 1);
                            continue;
                        }
                    };
                    compiler
                        .frame_mut()
                        .unwrap()
                        .write(ByteCode::Return { src: Some(src) }, Position::default());
                    compiler.pop_frame().unwrap().closure
                }
                Err(Located { value: err, pos }) => match parse_str::<Chunk>(&input) {
                    Ok(chunk) => match chunk.compile(&mut Compiler::default()) {
                        Ok(closure) => closure,
                        Err(Located { value: err, pos }) => {
                            eprintln!("ERROR {}:{}: {err:?}", pos.ln.start + 1, pos.col.start + 1);
                            continue;
                        }
                    },
                    Err(_) => {
                        eprintln!("ERROR {}:{}: {err}", pos.ln.start + 1, pos.col.start + 1);
                        continue;
                    }
                },
            };
            let function = Rc::new(Function {
                closure,
                upvalues: vec![],
            });
            interpreter.call(&function, vec![], None);
            match interpreter.run() {
                Ok(Some(value)) => println!("{value:?}"),
                Err(Located { value: err, pos }) => {
                    eprintln!("ERROR {}:{}: {err:?}", pos.ln.start + 1, pos.col.start + 1);
                    continue;
                }
                _ => {}
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct LunaArgs {
    path: Option<String>,

    tokens: bool,
    ast: bool,
    code: bool,
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
            return Err(LunaArgsError);
        }
        Ok(Self {
            path: if !singles.is_empty() {
                Some(singles.remove(0))
            } else {
                None
            },
            tokens: flags.contains("tokens") || flags.contains("t"),
            ast: flags.contains("ast") || flags.contains("a"),
            code: flags.contains("code") || flags.contains("c"),
        })
    }
}
