#![allow(unused_macros)]
use crate::{
    lang::value::{FunctionKind, Object, UserObject, UserObjectError, Value},
    run, LunaArgs,
};
use std::{
    cell::RefCell, collections::HashMap, error::Error, fmt::Display, fs, io::Write, ops::Range,
    rc::Rc, vec::IntoIter,
};

use super::interpreter::Interpreter;

macro_rules! int {
    ($v:literal) => {
        Value::Int($v.into())
    };
}
macro_rules! float {
    ($v:literal) => {
        Value::Float($v.into())
    };
}
macro_rules! bool {
    ($v:literal) => {
        Value::Bool($v.into())
    };
}
macro_rules! char {
    ($v:literal) => {
        Value::Char($v.into())
    };
}
macro_rules! string {
    ($v:literal) => {
        Value::String($v.to_string())
    };
}
macro_rules! vector {
    [$($v:literal),*] => {
        Value::Vector(Rc::new(RefCell::new(vec![$($v.into()),*])))
    };
}
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
    idx: usize,
    expected: Vec<&'static str>,
    got: &'static str,
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

pub fn globals() -> HashMap<String, Rc<RefCell<Value>>> {
    let mut globals = HashMap::new();
    set_field!(globals."print" = function!(_print));
    set_field!(globals."input" = function!(_input));
    set_field!(globals."assert" = function!(_assert));
    set_field!(globals."error" = function!(_error));
    set_field!(globals."safe_call" = function!(_safe_call));
    set_field!(globals."type" = function!(_type));
    set_field!(globals."require" = function!(_require));
    set_field!(globals."int" = object! {
        "from" = function!(_int_from)
        // "bytes" = function!(_int_bytes)
    });
    set_field!(globals."float" = object! {
        "from" = function!(_float_from),
        "floor" = function!(_float_floor),
        "ceil" = function!(_float_ceil),
        "round" = function!(_float_round)
    });
    set_field!(globals."bool" = object! {
        "from" = function!(_bool_from)
    });
    set_field!(globals."char" = object! {
        "from" = function!(_char_from),
        "byte" = function!(_char_byte),
        "is_whitespace" = function!(_char_is_whitespace),
        "is_alphabetic" = function!(_char_is_alphabetic),
        "is_alphanumeric" = function!(_char_is_alphanumeric),
        "is_control" = function!(_char_is_control),
        "is_digit" = function!(_char_is_digit),
        "is_graphic" = function!(_char_is_graphic),
        "is_hex" = function!(_char_is_hex),
        "is_lower" = function!(_char_is_lower),
        "is_upper" = function!(_char_is_upper)
    });
    set_field!(globals."str" = object! {
        "lowercase" = ('a'..='z').collect::<Vec<char>>(),
        "uppercase" = ('A'..='Z').collect::<Vec<char>>(),
        "letters" = ('a'..='z').chain('A'..='Z').collect::<Vec<char>>(),
        "from" = function!(_string_from),
        "iter" = function!(_string_iter),
        "get" = function!(_string_get),
        "sub" = function!(_string_sub),
        "sep" = function!(_string_sep),
        "rep" = function!(_string_rep),
        "rev" = function!(_string_rev),
        "find" = function!(_string_find),
        "format" = function!(_string_format)
    });
    set_field!(globals."vec" = object! {
        "iter" = function!(_vector_iter),
        "get" = function!(_vector_get),
        "contains" = function!(_vector_contains),
        "push" = function!(_vector_push),
        "pop" = function!(_vector_pop)
    });
    set_field!(globals."obj" = object! {
        "keys" = function!(_object_keys),
        "values" = function!(_object_values),
        "setmeta" = function!(_object_setmeta),
        "getmeta" = function!(_object_getmeta)
    });
    set_field!(globals."keys" = function!(_object_keys));
    set_field!(globals."values" = function!(_object_values));
    set_field!(globals."setmeta" = function!(_object_setmeta));
    set_field!(globals."getmeta" = function!(_object_getmeta));
    set_field!(globals."range" = function!(_range));
    set_field!(globals."math" = object! {
        "pi" = Value::Float(std::f64::consts::PI),
        "nan" = Value::Float(f64::NAN),
        "inf" = Value::Float(f64::INFINITY),
        "e" = Value::Float(f64::EPSILON),
        "abs" = function!(_math_abs),
        "sqrt" = function!(_math_sqrt),
        "exp" = function!(_math_exp),
        "exp2" = function!(_math_exp2),
        "exp_m1" = function!(_math_exp_m1),
        "signum" = function!(_math_signum),
        "fract" = function!(_math_fract),
        "cos" = function!(_math_cos),
        "sin" = function!(_math_sin),
        "tan" = function!(_math_tan),
        "cosh" = function!(_math_cosh),
        "sinh" = function!(_math_sinh),
        "tanh" = function!(_math_tanh),
        "acos" = function!(_math_acos),
        "asin" = function!(_math_asin),
        "atan" = function!(_math_atan),
        "acosh" = function!(_math_acosh),
        "asinh" = function!(_math_asinh),
        "atanh" = function!(_math_atanh),
        "deg" = function!(_math_deg),
        "rad" = function!(_math_rad),
        "random" = function!(_math_random)
    });
    // set_field!(globals."io" = object! {
    //     "write" = function!(_io_write),
    //     "open" = function!(_io_open),
    //     "close" = function!(_io_close),
    //     "stdin" = function!(_io_stdin),
    //     "stdout" = function!(_io_stdout),
    //     "stderr" = function!(_io_stderr)
    // });
    globals
}

pub fn _print(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let args = args.into_iter();

    println!(
        "{}",
        args.map(|v| v.to_string())
            .collect::<Vec<String>>()
            .join(" ")
    );
    Ok(Value::default())
}
pub fn _input(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let perfix = typed!(args: String?);

    let mut input = String::new();
    if let Some(prefix) = perfix {
        print!("{prefix}");
        std::io::stdout().flush()?;
    }
    std::io::stdin().read_line(&mut input)?;
    let input = input.trim_end();
    Ok(Value::String(input.to_string()))
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AssertError;
impl Display for AssertError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "assertion failed")
    }
}
impl Error for AssertError {}
pub fn _assert(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let cond = bool::from(args.next().unwrap_or_default().1);
    if cond {
        Ok(Value::default())
    } else {
        Err(Box::new(AssertError))
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct CustomError(String);
impl Display for CustomError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Error for CustomError {}
pub fn _error(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let msg = args.next().unwrap_or_default().1.to_string();
    Err(Box::new(CustomError(msg)))
}
pub fn _safe_call(
    interpreter: &mut Interpreter,
    args: Vec<Value>,
) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let func = typed!(args: Function);
    Ok(match func {
        FunctionKind::Function(func) => {
            interpreter.call(&func, args.map(|(_, v)| v).collect(), None);
            let res = interpreter.run();
            match res {
                Ok(value) => object! {
                    "ok" = value.unwrap_or_default()
                },
                Err(err) => object! {
                    "err" = Value::String(err.value.to_string())
                },
            }
        }
        FunctionKind::UserFunction(func) => match func(interpreter, args.map(|(_, v)| v).collect())
        {
            Ok(value) => object! {
                "ok" = value
            },
            Err(err) => object! {
                "err" = Value::String(err.to_string())
            },
        },
    })
}
pub fn _type(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = args.next().unwrap_or_default().1;
    Ok(Value::String(value.typ().to_string()))
}
#[derive(Debug, Clone, PartialEq)]
pub struct RequireError(String);
impl Display for RequireError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "couldn't find any of these modules:\n\t{}.luna\n\t{}/mod.luna",
            self.0, self.0
        )
    }
}
impl Error for RequireError {}
pub fn _require(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let path = typed!(args: String path => path.split('.').collect::<Vec<&str>>().join("/"));
    let (text, full_path) = if let Ok(text) = fs::read_to_string(format!("{path}.luna")) {
        (text, format!("{path}.luna"))
    } else if let Ok(text) = fs::read_to_string(format!("{path}/mod.luna")) {
        (text, format!("{path}/mod.luna"))
    } else {
        return Err(Box::new(RequireError(path)));
    };
    Ok(run(&text, &LunaArgs::default())
        .map_err(|err| {
            format!(
                "{full_path}:{}:{}: {}",
                err.pos.ln.start + 1,
                err.pos.col.start + 1,
                err.value
            )
        })?
        .unwrap_or_default())
}

pub fn _int_from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let (_, value) = args.next().unwrap_or_default();
    Ok(Value::Int(match value {
        Value::Int(v) => v,
        Value::Float(v) => v as i64,
        Value::Bool(v) => {
            if v {
                1
            } else {
                0
            }
        }
        Value::Char(v) => v as i64,
        Value::String(v) => {
            if let Ok(v) = v.parse() {
                v
            } else {
                return Ok(Value::default());
            }
        }
        _ => return Ok(Value::default()),
    }))
}

pub fn _float_from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let (_, value) = args.next().unwrap_or_default();
    Ok(Value::Float(match value {
        Value::Int(v) => v as f64,
        Value::Float(v) => v,
        Value::Bool(v) => {
            if v {
                1.
            } else {
                0.
            }
        }
        Value::String(v) => {
            if let Ok(v) = v.parse() {
                v
            } else {
                return Ok(Value::default());
            }
        }
        _ => return Ok(Value::default()),
    }))
}
pub fn _float_floor(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Float);
    Ok(Value::Float(value.floor()))
}
pub fn _float_ceil(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Float);
    Ok(Value::Float(value.ceil()))
}
pub fn _float_round(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Float);
    Ok(Value::Float(value.round()))
}

pub fn _bool_from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let (_, value) = args.next().unwrap_or_default();
    Ok(Value::Bool(value.into()))
}

pub fn _char_from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Int);
    Ok(if let Ok(v) = u8::try_from(value) {
        Value::Char(v as char)
    } else {
        Value::default()
    })
}
pub fn _char_byte(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    Ok(Value::Int(value as u8 as i64))
}
pub fn _char_is_whitespace(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    Ok(Value::Bool(value.is_ascii_whitespace()))
}
pub fn _char_is_alphabetic(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    Ok(Value::Bool(value.is_ascii_alphabetic()))
}
pub fn _char_is_alphanumeric(
    _: &mut Interpreter,
    args: Vec<Value>,
) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    Ok(Value::Bool(value.is_ascii_alphanumeric()))
}
pub fn _char_is_digit(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    let radix = typed!(args: Int?).and_then(|v| u32::try_from(v).ok());
    Ok(Value::Bool(value.is_digit(radix.unwrap_or(10))))
}
pub fn _char_is_control(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    Ok(Value::Bool(value.is_ascii_control()))
}
pub fn _char_is_graphic(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    Ok(Value::Bool(value.is_ascii_graphic()))
}
pub fn _char_is_hex(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    Ok(Value::Bool(value.is_ascii_hexdigit()))
}
pub fn _char_is_lower(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    Ok(Value::Bool(value.is_ascii_lowercase()))
}
pub fn _char_is_upper(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);
    Ok(Value::Bool(value.is_ascii_uppercase()))
}

pub fn _string_from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let args = args.into_iter().enumerate();
    Ok(Value::String(
        args.map(|(_, value)| value.to_string())
            .collect::<Vec<String>>()
            .join(""),
    ))
}
pub fn _string_iter(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        StringIterator(string.chars().collect::<Vec<char>>().into_iter()),
    )))))
}
pub fn _string_get(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
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
pub fn _string_sub(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _string_sep(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let sep = typed!(args: String);

    Ok(string
        .split(&sep)
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
        .into())
}
pub fn _string_rep(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    let n = typed!(args: Int);

    Ok(string.repeat(n as usize).into())
}
pub fn _string_rev(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);

    Ok(string.chars().rev().collect::<String>().into())
}
pub fn _string_find(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _string_format(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
                    's' => args.next().map(|(_, v)| v).unwrap_or_default().to_string(),
                    'q' => format!("{:?}", args.next().map(|(_, v)| v).unwrap_or_default()),
                    'x' => match args.next().map(|(_, v)| v).unwrap_or_default() {
                        Value::Int(v) => format!("{v:x?}"),
                        value => value.to_string()
                    }
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

pub fn _vector_iter(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let vector = vector.borrow();

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        VectorIterator(vector.clone().into_iter()),
    )))))
}
pub fn _vector_get(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let vector = vector.borrow();
    let index = typed!(args: Int);
    let default = args.next().map(|(_, v)| v);

    Ok(vector
        .get(index.unsigned_abs() as usize)
        .cloned()
        .unwrap_or(default.unwrap_or_default()))
}
pub fn _vector_contains(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let vector = vector.borrow();
    let value = args.next().map(|(_, v)| v).unwrap_or_default();

    Ok(vector.contains(&value).into())
}
pub fn _vector_push(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let mut vector = vector.borrow_mut();
    let value = args.next().map(|(_, v)| v).unwrap_or_default();

    vector.push(value);
    Ok(Value::default())
}
pub fn _vector_pop(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let mut vector = vector.borrow_mut();
    let index = typed!(args: Int?);

    if let Some(index) = index {
        let index = index.unsigned_abs() as usize;
        if vector.get(index).is_some() {
            Ok(vector.remove(index))
        } else {
            Ok(Value::default())
        }
    } else {
        Ok(vector.pop().unwrap_or_default())
    }
}

pub fn _object_keys(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let object = object.borrow();

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        ObjectKeysIterator(
            object
                .fields
                .keys()
                .cloned()
                .collect::<Vec<String>>()
                .into_iter(),
        ),
    )))))
}
pub fn _object_values(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let object = object.borrow();

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        ObjectValuesIterator(
            object
                .fields
                .values()
                .cloned()
                .collect::<Vec<Value>>()
                .into_iter(),
        ),
    )))))
}
pub fn _object_setmeta(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    {
        let mut object = object.borrow_mut();
        let meta = typed!(args: Object?);

        object.meta = meta;
    }
    Ok(Value::Object(object))
}
pub fn _object_getmeta(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let object = object.borrow();
    Ok(object
        .meta
        .as_ref()
        .map(|o| Value::Object(Rc::clone(o)))
        .unwrap_or_default())
}
pub fn _range(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let start = typed!(args: Int);
    let end = typed!(args: Int?);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        RangeIterator(if let Some(end) = end {
            start..end
        } else {
            0..start
        }),
    )))))
}

pub fn _math_abs(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Int(value.abs()))
        },
        Float => value {
            Ok(Value::Float(value.abs()))
        }
    )
}
pub fn _math_sqrt(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).sqrt()))
        },
        Float => value {
            Ok(Value::Float(value.sqrt()))
        }
    )
}
pub fn _math_exp(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).exp()))
        },
        Float => value {
            Ok(Value::Float(value.exp()))
        }
    )
}
pub fn _math_exp2(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).exp2()))
        },
        Float => value {
            Ok(Value::Float(value.exp2()))
        }
    )
}
pub fn _math_exp_m1(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).exp_m1()))
        },
        Float => value {
            Ok(Value::Float(value.exp_m1()))
        }
    )
}
pub fn _math_signum(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).signum()))
        },
        Float => value {
            Ok(Value::Float(value.signum()))
        }
    )
}
pub fn _math_fract(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).fract()))
        },
        Float => value {
            Ok(Value::Float(value.fract()))
        }
    )
}
pub fn _math_sin(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).sin()))
        },
        Float => value {
            Ok(Value::Float(value.sin()))
        }
    )
}
pub fn _math_cos(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).cos()))
        },
        Float => value {
            Ok(Value::Float(value.cos()))
        }
    )
}
pub fn _math_tan(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).tan()))
        },
        Float => value {
            Ok(Value::Float(value.tan()))
        }
    )
}
pub fn _math_sinh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).sinh()))
        },
        Float => value {
            Ok(Value::Float(value.sinh()))
        }
    )
}
pub fn _math_cosh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).cosh()))
        },
        Float => value {
            Ok(Value::Float(value.cosh()))
        }
    )
}
pub fn _math_tanh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).tanh()))
        },
        Float => value {
            Ok(Value::Float(value.tanh()))
        }
    )
}
pub fn _math_asin(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).asin()))
        },
        Float => value {
            Ok(Value::Float(value.asin()))
        }
    )
}
pub fn _math_acos(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).acos()))
        },
        Float => value {
            Ok(Value::Float(value.acos()))
        }
    )
}
pub fn _math_atan(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).atan()))
        },
        Float => value {
            Ok(Value::Float(value.atan()))
        }
    )
}
pub fn _math_asinh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).asinh()))
        },
        Float => value {
            Ok(Value::Float(value.asinh()))
        }
    )
}
pub fn _math_acosh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).acosh()))
        },
        Float => value {
            Ok(Value::Float(value.acosh()))
        }
    )
}
pub fn _math_atanh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).atanh()))
        },
        Float => value {
            Ok(Value::Float(value.atanh()))
        }
    )
}
pub fn _math_deg(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).to_degrees()))
        },
        Float => value {
            Ok(Value::Float(value.to_degrees()))
        }
    )
}
pub fn _math_rad(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).to_radians()))
        },
        Float => value {
            Ok(Value::Float(value.to_radians()))
        }
    )
}
pub fn _math_random(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::Float(rand::random()))
}

#[derive(Debug, Clone)]
pub struct VectorIterator(pub IntoIter<Value>);
#[derive(Debug, Clone)]
pub struct ObjectKeysIterator(pub IntoIter<String>);
#[derive(Debug, Clone)]
pub struct ObjectValuesIterator(pub IntoIter<Value>);
#[derive(Debug, Clone)]
pub struct StringIterator(pub IntoIter<char>);
#[derive(Debug, Clone)]
pub struct RangeIterator(pub Range<i64>);

impl Display for UserObjectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedSelf(got) => {
                write!(f, "expected self for argument #1, got {got}")
            }
            Self::CannotCallNull => {
                write!(f, "can not call null")
            }
            Self::InvalidField(field) => {
                write!(f, "invalid field {field:?}")
            }
        }
    }
}
impl Error for UserObjectError {}
impl UserObject for VectorIterator {
    fn typ(&self) -> &'static str {
        "vector-iter"
    }
    fn get(&self, key: &str) -> Option<Value> {
        match key {
            "next" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_next),
            )))),
            "collect" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_collect),
            )))),
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
    fn call(&self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "collect" => self.call_collect(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
}
impl VectorIterator {
    pub fn _next(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            _self.call_mut("next", args)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_next(&mut self) -> Result<Value, Box<dyn Error>> {
        Ok(self.0.next().unwrap_or_default())
    }
    pub fn _collect(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            _self.call("collect", args)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_collect(&self) -> Result<Value, Box<dyn Error>> {
        Ok(Value::Vector(Rc::new(RefCell::new(
            self.0.clone().collect::<Vec<Value>>(),
        ))))
    }
}
impl UserObject for ObjectKeysIterator {
    fn typ(&self) -> &'static str {
        "object-keys"
    }
    fn get(&self, key: &str) -> Option<Value> {
        match key {
            "next" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_next),
            )))),
            "collect" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_collect),
            )))),
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
    fn call(&self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "collect" => self.call_collect(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
}
impl ObjectKeysIterator {
    pub fn _next(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            Ok(_self.call_mut("next", args)?)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_next(&mut self) -> Result<Value, Box<dyn Error>> {
        Ok(self.0.next().map(Value::String).unwrap_or_default())
    }
    pub fn _collect(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            _self.call("collect", args)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_collect(&self) -> Result<Value, Box<dyn Error>> {
        Ok(Value::Vector(Rc::new(RefCell::new(
            self.0.clone().map(Value::String).collect::<Vec<Value>>(),
        ))))
    }
}
impl UserObject for ObjectValuesIterator {
    fn typ(&self) -> &'static str {
        "object-keys"
    }
    fn get(&self, key: &str) -> Option<Value> {
        match key {
            "next" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_next),
            )))),
            "collect" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_collect),
            )))),
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
    fn call(&self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "collect" => self.call_collect(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
}
impl ObjectValuesIterator {
    pub fn _next(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            Ok(_self.call_mut("next", args)?)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_next(&mut self) -> Result<Value, Box<dyn Error>> {
        Ok(self.0.next().unwrap_or_default())
    }
    pub fn _collect(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            _self.call("collect", args)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_collect(&self) -> Result<Value, Box<dyn Error>> {
        Ok(Value::Vector(Rc::new(RefCell::new(
            self.0.clone().collect::<Vec<Value>>(),
        ))))
    }
}
impl UserObject for StringIterator {
    fn typ(&self) -> &'static str {
        "string-iter"
    }
    fn get(&self, key: &str) -> Option<Value> {
        match key {
            "next" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_next),
            )))),
            "collect" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_collect),
            )))),
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
    fn call(&self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "collect" => self.call_collect(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
}
impl StringIterator {
    pub fn _next(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            _self.call_mut("next", args)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_next(&mut self) -> Result<Value, Box<dyn Error>> {
        Ok(self.0.next().map(Value::Char).unwrap_or_default())
    }
    pub fn _collect(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            _self.call("collect", args)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_collect(&self) -> Result<Value, Box<dyn Error>> {
        Ok(Value::Vector(Rc::new(RefCell::new(
            self.0.clone().map(Value::Char).collect::<Vec<Value>>(),
        ))))
    }
}
impl UserObject for RangeIterator {
    fn typ(&self) -> &'static str {
        "range-iter"
    }
    fn get(&self, key: &str) -> Option<Value> {
        match key {
            "next" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_next),
            )))),
            "collect" => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                Box::new(Self::_collect),
            )))),
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
    fn call(&self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "collect" => self.call_collect(),
            _ => Err(Box::new(UserObjectError::CannotCallNull)),
        }
    }
}
impl RangeIterator {
    pub fn _next(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            _self.call_mut("next", args)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_next(&mut self) -> Result<Value, Box<dyn Error>> {
        Ok(self.0.next().map(Value::Int).unwrap_or_default())
    }
    pub fn _collect(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        let Some(_self) = args.first().cloned() else {
            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
        };
        if let Value::UserObject(_self) = _self {
            let mut _self = _self.borrow_mut();
            _self.call("collect", args)
        } else {
            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
        }
    }
    pub fn call_collect(&self) -> Result<Value, Box<dyn Error>> {
        Ok(Value::Vector(Rc::new(RefCell::new(
            self.0.clone().map(Value::Int).collect::<Vec<Value>>(),
        ))))
    }
}
