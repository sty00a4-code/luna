#![allow(unused_macros)]
use crate::lang::value::{FunctionKind, Object, UserObject, UserObjectError, Value};
use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, io::Write, ops::Range, rc::Rc, vec::IntoIter};

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
    ($args:ident : $type:ident $param:ident) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if let Value::$type($param) = arg {
            $param
        } else {
            return Err(ExpectedType {
                idx,
                expected: Value::$type(Default::default()).typ(),
                got: arg.typ(),
            }
            .into());
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
    ($args:ident : $type:ident ? $param:ident) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if arg == Value::default() {
            None
        } else {
            if let Value::$type($param) = arg {
                Some($param)
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

pub fn globals() -> HashMap<String, Rc<RefCell<Value>>> {
    let mut globals = HashMap::new();
    set_field!(globals."print" = function!(_print));
    set_field!(globals."input" = function!(_input));
    set_field!(globals."assert" = function!(_assert));
    set_field!(globals."int" = object! {
        "from" = function!(_int_from)
    });
    set_field!(globals."float" = object! {
        "from" = function!(_float_from)
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
    set_field!(globals."string" = object! {
        "lowercase" = ('a'..='z').collect::<Vec<char>>(),
        "uppercase" = ('A'..='Z').collect::<Vec<char>>(),
        "letters" = ('a'..='z').chain('A'..='Z').collect::<Vec<char>>(),
        "from" = function!(_string_from),
        "iter" = function!(_string_iter),
        "get" = function!(_string_get),
        "sub" = function!(_string_sub),
        "sep" = function!(_string_sep)
    });
    set_field!(globals."vector" = object! {
        "iter" = function!(_vector_iter),
        "get" = function!(_vector_get),
        "contains" = function!(_vector_contains),
        "push" = function!(_vector_push),
        "pop" = function!(_vector_pop)
    });
    set_field!(globals."object" = object! {
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

pub fn _int_from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let (_, value) = args.next().unwrap_or_default();
    Ok(Value::Int(match value {
        Value::Int(v) => v,
        Value::Float(v) => v as i64,
        Value::Bool(v) => if v { 1 } else { 0 },
        Value::Char(v) => v as i64,
        Value::String(v) => if let Ok(v) = v.parse() {
            v
        } else {
            return Ok(Value::default())
        },
        _ => return Ok(Value::default())
    }))
}

pub fn _float_from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let (_, value) = args.next().unwrap_or_default();
    Ok(Value::Float(match value {
        Value::Int(v) => v as f64,
        Value::Float(v) => v,
        Value::Bool(v) => if v { 1. } else { 0. },
        Value::String(v) => if let Ok(v) = v.parse() {
            v
        } else {
            return Ok(Value::default())
        },
        _ => return Ok(Value::default())
    }))
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
pub fn _char_is_alphanumeric(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
    Ok(Value::String(args.map(|(_, value)| value.to_string()).collect::<Vec<String>>().join("")))
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
        ObjectKeysIterator(object.fields.keys().cloned().collect::<Vec<String>>().into_iter()),
    )))))
}
pub fn _object_values(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let object = object.borrow();

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        ObjectValuesIterator(object.fields.values().cloned().collect::<Vec<Value>>().into_iter()),
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
    Ok(object.meta.as_ref().map(|o| Value::Object(Rc::clone(o))).unwrap_or_default())
}
pub fn _range(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let start = typed!(args: Int);
    let end = typed!(args: Int?);
    
    Ok(Value::UserObject(Rc::new(RefCell::new(
        Box::new(RangeIterator(if let Some(end) = end {
            start..end
        } else {
            0..start
        }))
    ))))
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
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
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
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
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
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
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
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
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
            _ => None,
        }
    }
    fn call_mut(&mut self, key: &str, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match key {
            "next" => self.call_next(),
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
}