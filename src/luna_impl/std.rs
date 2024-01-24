#![allow(unused_macros)]
use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, rc::Rc};
use crate::lang::value::{FunctionKind, Value};

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
    expected: &'static str,
    got: &'static str,
}
impl Display for ExpectedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected {}, got {}", self.expected, self.got)
    }
}
impl Error for ExpectedType {}
macro_rules! make_function {
    ($name:ident ( $($type:ident $param:pat),* ) $code:block) => {
        pub fn $name(args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
            #[allow(unused_variables, unused_mut)]
            let mut args = args.into_iter();
            $(
                let arg = args.next().unwrap_or_default();
                let Value::$type($param) = arg else {
                    return Err(ExpectedType {
                        expected: Value::$type(Default::default()).typ(),
                        got: arg.typ()
                    }.into())
                };
            ) *
            $code
        }
    };
}

pub fn globals() -> HashMap<String, Rc<RefCell<Value>>> {
    let mut globals = HashMap::new();
    set_field!(globals."print" = function!(_print));
    globals
    
}
make_function! { _print ( String string ) {
    println!("{}", string);
    Ok(Value::default())
} }