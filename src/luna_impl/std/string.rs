use super::{int, IteratorObject};
use crate::{
    function,
    lang::{interpreter::Interpreter, value::{Value, Object, FunctionKind}},
    luna_impl::std::STRING_MODULE,
    object, set_field, typed, ExpectedType,
};
use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, rc::Rc};

pub fn define(globals: &mut HashMap<String, Rc<RefCell<Value>>>) {
    set_field!(globals."str" = function!(_from));
    set_field!(
        globals.STRING_MODULE = object! {
            "lowercase" = ('a'..='z').collect::<Vec<char>>(),
            "uppercase" = ('A'..='Z').collect::<Vec<char>>(),
            "letters" = ('a'..='z').chain('A'..='Z').collect::<Vec<char>>(),
            "from" = globals["str"].borrow().clone(),
            "len" = function!(_len),
            "iter" = function!(_iter),
            "get" = function!(_get),
            "sub" = function!(_sub),
            "split" = function!(_split),
            "split_amount" = function!(_split_amount),
            "split_at" = function!(_split_at),
            "split_off" = function!(_split_off),
            "rep" = function!(_rep),
            "rev" = function!(_rev),
            "find" = function!(_find),
            "format" = function!(_format),
            "bin" = function!(int::_from_bin),
            "hex" = function!(int::_from_hex),
            "start" = function!(_starts_with),
            "end" = function!(_ends_with)
        }
    );
}

pub fn _from(interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let args = args.into_iter().enumerate();
    let mut string = String::new();
    for (_, value) in args {
        string.push_str(
            &value
                .call_tostring(interpreter)
                .map_err(|err| Into::<Box<dyn Error>>::into(err.to_string()))?,
        );
    }
    Ok(Value::String(string))
}
pub fn _iter(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        IteratorObject(Box::new(
            string
                .chars()
                .map(Value::Char)
                .collect::<Vec<Value>>()
                .into_iter(),
        )),
    )))))
}
pub fn _len(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);

    Ok(Value::Int(string.len() as i64))
}
pub fn _get(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args: std::iter::Enumerate<std::vec::IntoIter<Value>> = args.into_iter().enumerate();
    let string = typed!(args: String);
    let index = typed!(args: Int);
    let default = typed!(args: Char?);

    Ok(string
        .chars()
        .nth(index.unsigned_abs() as usize)
        .map(Value::Char)
        .or(default.map(Value::Char))
        .unwrap_or_default())
}
pub fn _sub(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let start = typed!(args: Int).unsigned_abs() as usize;
    let stop = typed!(args: Int?)
        .map(|idx| idx.unsigned_abs() as usize)
        .unwrap_or(string.len() - 1);
    let default = typed!(args: String?);

    Ok(string
        .get(start..=stop)
        .map(|slice| Value::String(slice.to_string()))
        .unwrap_or(default.map(Value::String).unwrap_or_default()))
}
pub fn _split(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let sep = typed!(args: String);

    Ok(string
        .split(&sep)
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
        .into())
}
pub fn _split_amount(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let n = typed!(args: Int);
    let sep = typed!(args: String);

    Ok(string
        .splitn(n.unsigned_abs() as usize, &sep)
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
        .into())
}
pub fn _split_at(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let n = typed!(args: Int);
    let (left, right) = string.split_at(n.unsigned_abs() as usize);
    Ok(vec![
        Value::String(left.to_string()),
        Value::String(right.to_string()),
    ]
    .into())
}
pub fn _split_off(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let mut string = typed!(args: String);
    let n = typed!(args: Int);
    Ok(string.split_off(n.unsigned_abs() as usize).into())
}
pub fn _rep(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let n = typed!(args: Int);

    Ok(string.repeat(n as usize).into())
}
pub fn _rev(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);

    Ok(string.chars().rev().collect::<String>().into())
}
pub fn _find(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let pattern = typed!(args: String);

    Ok(string
        .find(&pattern)
        .map(|i| Value::Int(i as i64))
        .unwrap_or_default())
}
#[derive(Debug, Clone, PartialEq)]
pub enum FormatErrorKind {
    NoFormatOption,
    InvalidFormatOption(char),
}
#[derive(Debug, Clone, PartialEq)]
pub struct FormatError {
    kind: FormatErrorKind,
    pos: usize,
}
impl Display for FormatErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatErrorKind::NoFormatOption => write!(f, "no format option"),
            FormatErrorKind::InvalidFormatOption(c) => write!(f, "invalid format option {c:?}"),
        }
    }
}
impl Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (at idx {})", self.kind, self.pos)
    }
}
impl Error for FormatError {}
pub fn _format(interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let mut formatted = String::new();
    let mut chars = string.chars().enumerate();
    while let Some((i, c)) = chars.next() {
        match c {
            '%' => {
                let Some((i, c)) = chars.next() else {
                    return Err(Box::new(FormatError {
                        kind: FormatErrorKind::NoFormatOption,
                        pos: i,
                    }));
                };
                formatted.push_str(&match c {
                    '%' => "%".to_string(),
                    's' => args
                        .next()
                        .map(|(_, v)| v)
                        .unwrap_or_default()
                        .call_tostring(interpreter)
                        .map_err(|err| Into::<Box<dyn Error>>::into(err.to_string()))?,
                    'q' => match args.next().map(|(_, v)| v).unwrap_or_default() {
                        Value::String(v) => format!("{v:?}"),
                        value => value
                            .call_tostring(interpreter)
                            .map_err(|err| Into::<Box<dyn Error>>::into(err.to_string()))?,
                    },
                    'x' => match args.next().map(|(_, v)| v).unwrap_or_default() {
                        Value::Int(v) => format!("{v:x?}"),
                        value => value
                            .call_tostring(interpreter)
                            .map_err(|err| Into::<Box<dyn Error>>::into(err.to_string()))?,
                    },
                    c => {
                        return Err(Box::new(FormatError {
                            kind: FormatErrorKind::InvalidFormatOption(c),
                            pos: i,
                        }))
                    }
                })
            }
            c => formatted.push(c),
        }
    }
    Ok(formatted.into())
}
pub fn _starts_with(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let start = typed!(args: String);

    Ok(string.starts_with(&start).into())
}
pub fn _ends_with(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let start = typed!(args: String);

    Ok(string.ends_with(&start).into())
}
