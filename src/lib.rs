pub mod lang;
pub mod luna_impl;
#[cfg(test)]
pub mod tests;

use std::{cell::RefCell, env, error::Error, fmt::Display, rc::Rc};

use lang::{ast::Chunk, code::Closure, tokens::Token, value::{Function, Value}};
use luna_impl::{compiler::{Compilable, Compiler}, interpreter::Interpreter, lexer::Lexer, parser::Parsable, position::Located};

pub fn lex_str(text: &str) -> Result<Vec<Located<Token>>, Located<Box<dyn Error>>> {
    Lexer::from(text)
        .lex()
        .map_err(|err| err.map(|err| err.into()))
}
pub fn parse_str(text: &str) -> Result<Located<Chunk>, Located<Box<dyn Error>>> {
    Chunk::parse(&mut lex_str(text)?.into_iter().peekable())
        .map_err(|err| err.map(|err| err.into()))
}
pub fn compile_str(text: &str) -> Result<Rc<RefCell<Closure>>, Located<Box<dyn Error>>> {
    let ast = parse_str(text)?;
    let closure = ast.compile(&mut Compiler::default())?;
    Ok(closure)
}
pub fn run_str(text: &str) -> Result<Option<Value>, Located<Box<dyn Error>>> {
    let closure = compile_str(text)?;
    let function = Rc::new(Function {
        closure,
        upvalues: vec![],
    });
    let mut interpreter = Interpreter::default().with_global_path(env::var("LUNA_PATH").ok());
    interpreter.call(&function, vec![], None);
    interpreter.run().map_err(|err| err.map(|err| err.into()))
}

#[macro_export]
macro_rules! int {
    ($v:literal) => {
        Value::Int($v.into())
    };
}
#[macro_export]
macro_rules! float {
    ($v:literal) => {
        Value::Float($v.into())
    };
}
#[macro_export]
macro_rules! bool {
    ($v:literal) => {
        Value::Bool($v.into())
    };
}
#[macro_export]
macro_rules! char {
    ($v:literal) => {
        Value::Char($v.into())
    };
}
#[macro_export]
macro_rules! string {
    ($v:literal) => {
        Value::String($v.to_string())
    };
}
#[macro_export]
macro_rules! vector {
    [$($v:literal),*] => {
        Value::Vector(Rc::new(RefCell::new(vec![$($v.into()),*])))
    };
}
#[macro_export]
macro_rules! object {
    {$($k:literal = $v:expr),*} => {
        {
            #[allow(unused_variables, unused_mut)]
            let mut map = HashMap::new();
            $(
                map.insert($k.into(), $v.into());
            ) *
            Value::Object(Rc::new(RefCell::new(Object::new(map))))
        }
    };
}
#[macro_export]
macro_rules! function {
    ($name:ident) => {
        Value::Function(FunctionKind::UserFunction(Rc::new($name)))
    };
}
#[macro_export]
macro_rules! set_field {
    ($map:ident . $field:literal = $value:expr) => {
        $map.insert($field.to_string(), Rc::new(RefCell::new($value)));
    };
}
#[derive(Debug, Clone, PartialEq)]
pub struct ExpectedType {
    pub idx: usize,
    pub expected: &'static str,
    pub got: &'static str,
}
impl Display for ExpectedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected {} for argument #{}, got {}",
            self.expected, self.idx, self.got
        )
    }
}
impl Error for ExpectedType {}
#[derive(Debug, Clone, PartialEq)]
pub struct ExpectedTypes {
    pub idx: usize,
    pub expected: Vec<&'static str>,
    pub got: &'static str,
}
impl Display for ExpectedTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected {} for argument #{}, got {}",
            self.expected.join("/"),
            self.idx,
            self.got
        )
    }
}
impl Error for ExpectedTypes {}
#[macro_export]
macro_rules! typed {
    ($args:ident : $type:ident) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if let Value::$type(value) = arg {
            value
        } else {
            return Err(ExpectedType {
                idx,
                expected: Value::$type(Default::default()).typ(),
                got: arg.typ(),
            }
            .into());
        }
    }};
    ($args:ident : $type:ident ?) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if arg == Value::default() {
            None
        } else {
            if let Value::$type(value) = arg {
                Some(value)
            } else {
                return Err(ExpectedType {
                    idx,
                    expected: Value::$type(Default::default()).typ(),
                    got: arg.typ(),
                }
                .into());
            }
        }
    }};
    ($args:ident : $type:ident $param:ident => $value:expr) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if let Value::$type($param) = arg {
            $value
        } else {
            return Err(ExpectedType {
                idx,
                expected: Value::$type(Default::default()).typ(),
                got: arg.typ(),
            }
            .into());
        }
    }};
    ($args:ident : $type:ident ? $param:ident => $value:expr) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if arg == Value::default() {
            None
        } else {
            if let Value::$type($param) = arg {
                Some($value)
            } else {
                return Err(ExpectedType {
                    idx,
                    expected: Value::$type(Default::default()).typ(),
                    got: arg.typ(),
                }
                .into());
            }
        }
    }};
}
#[macro_export]
macro_rules! option {
    ($args:ident : $($type:ident => $value:ident $body:block),+) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        match arg {
            $(
                Value::$type($value) => $body,
            ) +
            arg => {
                return Err(ExpectedTypes {
                    idx,
                    expected: vec![$(Value::$type(Default::default()).typ()),+],
                    got: arg.typ(),
                }
                .into())
            }
        }
    }};
}