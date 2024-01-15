use std::{rc::Rc, collections::HashMap, cell::RefCell, slice::Iter, str::Chars};
use super::code::Closure;

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
    String(String),
    Vector(Rc<RefCell<Vec<Self>>>),
    Object(Rc<RefCell<Object>>),
    UserObject(Rc<RefCell<Box<dyn UserObject>>>),
    Function(FunctionKind),
}
#[derive(Clone)]
pub struct Object {
    pub fields: HashMap<String, Value>,
    pub meta: Option<Rc<RefCell<Self>>>
}
pub trait UserObject {
    fn name(&self) -> &'static str;
    fn get(&self, key: &str) -> Option<&Value>;
    fn get_mut(&mut self, key: &str) -> Option<&mut Value>;
    fn get_meta(&self, key: &str) -> Option<&Value>;
    fn get_meta_mut(&mut self, key: &str) -> Option<&mut Value>;
    fn set(&mut self, key: &str, value: Value);
}
#[derive(Clone)]
pub enum FunctionKind {
    Function(Rc<Closure>),
    UserFunction(Rc<UserFunction>),
}
pub type UserFunction = Box<dyn Fn(Vec<Value>) -> Option<Value>>;

#[derive(Clone)]
pub struct VectorIterator<'a>(Iter<'a, Value>);
#[derive(Clone)]
pub struct ObjectIterator<'a>(Iter<'a, (String, Value)>);
#[derive(Clone)]
pub struct StringIterator<'a>(Chars<'a>);