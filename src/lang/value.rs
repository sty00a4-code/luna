use super::code::Closure;
use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
    rc::Rc,
    slice::Iter,
    str::Chars,
};

pub const META_NAME: &str = "__name";
pub const META_TYPE: &str = "__type";
pub const META_TOSTRING: &str = "__tostring";
pub const META_CALL: &str = "__call";
pub const META_GET: &str = "__get";
pub const META_SET: &str = "__set";

#[derive(Clone, Default)]
pub enum Value {
    #[default]
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Vector(Rc<RefCell<Vec<Self>>>),
    Object(Rc<RefCell<Object>>),
    UserObject(Rc<RefCell<Box<dyn UserObject>>>),
    Function(FunctionKind),
}
#[derive(Debug, Clone, Default)]
pub struct Object {
    pub fields: HashMap<String, Value>,
    pub meta: Option<Rc<RefCell<Self>>>,
}
pub trait UserObject {
    fn typ(&self) -> &'static str;
    #[allow(unused_variables)]
    fn get(&self, key: &str) -> Option<Value> {
        None
    }
    #[allow(unused_variables)]
    fn set(&mut self, key: &str, value: Value) -> Result<(), UserObjectError> {
        Err(UserObjectError::InvalidField(key.into()))
    }
    #[allow(unused_variables)]
    fn call(&self, key: &str, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        Err(Box::new(UserObjectError::CannotCallNull))
    }
    #[allow(unused_variables)]
    fn call_mut(&mut self, key: &str, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        Err(Box::new(UserObjectError::CannotCallNull))
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum UserObjectError {
    ExpectedSelf(&'static str),
    CannotCallNull,
    InvalidField(String),
}
#[derive(Clone)]
pub enum FunctionKind {
    Function(Rc<Function>),
    UserFunction(Rc<UserFunction>),
}
#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) closure: Rc<RefCell<Closure>>,
    pub(crate) upvalues: Vec<Rc<RefCell<Value>>>
}
pub type UserFunction = Box<dyn Fn(Vec<Value>) -> Result<Value, Box<dyn Error>>>;

impl Value {
    pub fn typ(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Bool(_) => "bool",
            Value::Char(_) => "char",
            Value::String(_) => "string",
            Value::Vector(_) => "vector",
            Value::Object(_) => "object",
            Value::UserObject(object) => object.borrow().typ(),
            Value::Function(_) => "fn",
        }
    }
}
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Int(v) => write!(f, "{v:?}"),
            Value::Float(v) => write!(f, "{v:?}"),
            Value::Bool(v) => write!(f, "{v:?}"),
            Value::Char(v) => write!(f, "{v:?}"),
            Value::String(v) => write!(f, "{v:?}"),
            Value::Vector(v) => write!(f, "{v:?}"),
            Value::Object(v) => write!(f, "object:{:08x?}", v.as_ptr()),
            Value::UserObject(v) => write!(f, "{}:{:08x?}", v.borrow().typ(), v.as_ptr()),
            Value::Function(FunctionKind::Function(function)) => {
                write!(f, "fn:{:08x?}", Rc::as_ptr(function))
            }
            Value::Function(FunctionKind::UserFunction(func)) => {
                write!(f, "fn:{:08x?}", Rc::as_ptr(func))
            }
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::Char(v) => write!(f, "{v}"),
            Value::String(v) => write!(f, "{v}"),
            _ => write!(f, "{self:?}"),
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Int(a), Self::Float(b)) => *a as f64 == *b,
            (Self::Float(a), Self::Int(b)) => *a == *b as f64,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Char(a), Self::Char(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Vector(a), Self::Vector(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
            (Self::Object(a), Self::Object(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
            (Self::UserObject(a), Self::UserObject(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
            (
                Self::Function(FunctionKind::Function(a)),
                Self::Function(FunctionKind::Function(b)),
            ) => Rc::as_ptr(a) == Rc::as_ptr(b),
            (
                Self::Function(FunctionKind::UserFunction(a)),
                Self::Function(FunctionKind::UserFunction(b)),
            ) => Rc::as_ptr(a) == Rc::as_ptr(b),
            _ => false,
        }
    }
}
impl From<Value> for bool {
    fn from(value: Value) -> Self {
        match value {
            Value::Null => false,
            Value::Int(v) => v != 0,
            Value::Float(v) => v != 0.,
            Value::Bool(v) => v,
            Value::Char(v) => v != 0 as char,
            Value::String(string) => !string.is_empty(),
            Value::Vector(vector) => !vector.borrow().is_empty(),
            Value::Object(_) => true,
            Value::UserObject(_) => true,
            Value::Function(_) => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VectorIterator<'a>(Iter<'a, Value>);
#[derive(Debug, Clone)]
pub struct ObjectIterator<'a>(Iter<'a, String>);
#[derive(Debug, Clone)]
pub struct StringIterator<'a>(Chars<'a>);

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
impl<'a> UserObject for VectorIterator<'a> {
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
impl<'a> VectorIterator<'a> {
    pub fn _next(args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
        Ok(self.0.next().cloned().unwrap_or_default())
    }
}
impl<'a> UserObject for ObjectIterator<'a> {
    fn typ(&self) -> &'static str {
        "object-iter"
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
impl<'a> ObjectIterator<'a> {
    pub fn _next(args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
        Ok(self
            .0
            .next()
            .cloned()
            .map(Value::String)
            .unwrap_or_default())
    }
}
impl<'a> UserObject for StringIterator<'a> {
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
impl<'a> StringIterator<'a> {
    pub fn _next(args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
