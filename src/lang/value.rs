use crate::luna_impl::{
    interpreter::{Interpreter, RunTimeError},
    position::Located,
};

use super::code::Closure;
use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
    rc::Rc,
};

pub const META_NAME: &str = "__name";
pub const META_TYPE: &str = "__type";
pub const META_TOSTRING: &str = "__tostring";
pub const META_CALL: &str = "__call";
pub const META_GET: &str = "__get";
pub const META_SET: &str = "__set";
pub const META_NEXT: &str = "__next";

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
#[derive(Debug, Clone, Default)]
pub struct Function {
    pub closure: Rc<RefCell<Closure>>,
    pub upvalues: Vec<Rc<RefCell<Value>>>,
}
pub type UserFunction = dyn Fn(&mut Interpreter, Vec<Value>) -> Result<Value, Box<dyn Error>>;

impl Default for FunctionKind {
    fn default() -> Self {
        Self::Function(Default::default())
    }
}
impl Object {
    pub fn new(fields: HashMap<String, Value>) -> Self {
        Self { fields, meta: None }
    }
    pub fn get(&self, k: &str) -> Option<Value> {
        self.fields.get(k).cloned()
    }
    pub fn set(&mut self, k: String, v: Value) {
        self.fields.insert(k, v);
    }
    pub fn get_meta(&self, k: &str) -> Option<Value> {
        self.meta.as_ref()?.borrow().get(k)
    }
}
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
    pub fn dynamic_typ(&self) -> String {
        match self {
            Value::Object(object) => {
                let object = object.borrow();
                if let Some(value) = object.get_meta(META_TYPE) {
                    value.to_string()
                } else {
                    self.typ().to_string()
                }
            }
            _ => self.typ().to_string()
        }
    }
    pub fn call_tostring(
        &self,
        interpreter: &mut Interpreter,
    ) -> Result<String, Located<RunTimeError>> {
        match self {
            Value::Object(object) => {
                let func = object.borrow().get_meta(META_TOSTRING);
                if let Some(Value::Function(FunctionKind::Function(func))) = func {
                    interpreter.call(&func, vec![self.clone()], None);
                    let value = interpreter.run()?;
                    Ok(value.unwrap_or_default().to_string())
                } else {
                    Ok(self.to_string())
                }
            }
            value => Ok(value.to_string()),
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
            Value::Vector(v) => write!(f, "{:?}", v.borrow()),
            Value::Object(v) => write!(
                f,
                "{}:{:08x?}",
                if let Some(value) = v.borrow().get_meta(META_TYPE) {
                    value.to_string()
                } else if let Some(value) = v.borrow().get_meta(META_NAME) {
                    value.to_string()
                } else {
                    "object".to_string()
                },
                v.as_ptr()
            ),
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
            ) => std::ptr::eq(a, b),
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

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Self::Int(value as i64)
    }
}
impl From<u16> for Value {
    fn from(value: u16) -> Self {
        Self::Int(value as i64)
    }
}
impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Self::Int(value as i64)
    }
}
impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Self::Int(value as i64)
    }
}
impl From<u128> for Value {
    fn from(value: u128) -> Self {
        Self::Int(value as i64)
    }
}
impl From<i8> for Value {
    fn from(value: i8) -> Self {
        Self::Int(value as i64)
    }
}
impl From<i16> for Value {
    fn from(value: i16) -> Self {
        Self::Int(value as i64)
    }
}
impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self::Int(value as i64)
    }
}
impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}
impl From<i128> for Value {
    fn from(value: i128) -> Self {
        Self::Int(value as i64)
    }
}
impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Self::Float(value as f64)
    }
}
impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}
impl From<char> for Value {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}
impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}
impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}
impl<V: Into<Value>> From<Vec<V>> for Value {
    fn from(value: Vec<V>) -> Self {
        Self::Vector(Rc::new(RefCell::new(
            value.into_iter().map(|v| v.into()).collect(),
        )))
    }
}
impl<V: Into<Value>> From<HashMap<String, V>> for Value {
    fn from(value: HashMap<String, V>) -> Self {
        Self::Object(Rc::new(RefCell::new(Object::new(
            value.into_iter().map(|(k, v)| (k, v.into())).collect(),
        ))))
    }
}
impl From<Rc<UserFunction>> for Value {
    fn from(value: Rc<UserFunction>) -> Self {
        Self::Function(FunctionKind::UserFunction(value))
    }
}
impl From<Box<UserFunction>> for Value {
    fn from(value: Box<UserFunction>) -> Self {
        Self::Function(FunctionKind::UserFunction(Rc::new(value)))
    }
}
