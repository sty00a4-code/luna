#![allow(unused_macros)]
use crate::lang::value::{FunctionKind, Value};
use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, io::Write, rc::Rc};

use super::interpreter::Interpreter;

macro_rules! string {
    ($v:literal) => {
        Value::String($v.to_string())
    };
}
macro_rules! function {
    ($name:ident) => {
        Value::Function(FunctionKind::UserFunction(Rc::new($name)))
    };
}
macro_rules! set_field {
    ($map:ident . $field:literal = $value:expr) => {
        $map.insert($field.to_string(), Rc::new(RefCell::new($value)));
    };
}
#[derive(Debug, Clone, PartialEq)]
pub struct ExpectedType {
    idx: usize,
    expected: &'static str,
    got: &'static str,
}
impl Display for ExpectedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected {} for argument #{}, got {}", self.expected, self.idx, self.got)
    }
}
impl Error for ExpectedType {}
macro_rules! typed {
    ($args:ident : $type:ident $param:ident) => {
        {
            let (idx, arg) = $args.next().unwrap_or_default();
            if let Value::$type($param) = arg {
                $param
            } else {
                return Err(ExpectedType {
                    idx,
                    expected: Value::$type(Default::default()).typ(),
                    got: arg.typ()
                }.into())
            }
        }
    };
    ($args:ident : $type:ident $param:ident => $value:expr) => {
        {
            let (idx, arg) = $args.next().unwrap_or_default();
            if let Value::$type($param) = arg {
                $value
            } else {
                return Err(ExpectedType {
                    idx,
                    expected: Value::$type(Default::default()).typ(),
                    got: arg.typ()
                }.into())
            }
        }
    };
    ($args:ident : $type:ident ? $param:ident) => {
        {
            let (idx, arg) = $args.next().unwrap_or_default();
            if arg == Value::default() {
                None
            } else {
                if let Value::$type($param) = arg {
                    Some($param)
                } else {
                    return Err(ExpectedType {
                        idx,
                        expected: Value::$type(Default::default()).typ(),
                        got: arg.typ()
                    }.into())
                }
            }
        }
    };
    ($args:ident : $type:ident ? $param:ident => $value:expr) => {
        {
            let (idx, arg) = $args.next().unwrap_or_default();
            if arg == Value::default() {
                None
            } else {
                if let Value::$type($param) = arg {
                    Some($value)
                } else {
                    return Err(ExpectedType {
                        idx,
                        expected: Value::$type(Default::default()).typ(),
                        got: arg.typ()
                    }.into())
                }
            }
        }
    };
}

pub fn globals() -> HashMap<String, Rc<RefCell<Value>>> {
    let mut globals = HashMap::new();
    set_field!(globals."print" = function!(_print));
    set_field!(globals."input" = function!(_input));
    globals
}

pub fn _print(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let args = args.into_iter();
    
    println!("{}", args.map(|v| v.to_string()).collect::<Vec<String>>().join(" "));
    Ok(Value::default())
}
pub fn _input(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let perfix = typed!(args: String? string);

    let mut input = String::new();
    if let Some(prefix) = perfix {
        print!("{prefix}");
        std::io::stdout().flush()?;
    }
    std::io::stdin().read_line(&mut input)?;
    let input = input.trim_end();
    Ok(Value::String(input.to_string()))
}