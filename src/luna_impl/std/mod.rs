#![allow(unused_macros)]
use super::position::Located;
use crate::{
    function,
    lang::{
        interpreter::{Interpreter, RunTimeError},
        value::{FunctionKind, Object, UserObject, UserObjectError, Value, META_NEXT},
    },
    luna_impl::ast::Chunk,
    object, run_str, set_field, typed, userobject, ExpectedType,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    env,
    error::Error,
    fmt::{Debug, Display},
    fs::{self},
    io::Write,
    path::Path,
    rc::Rc,
};

pub mod bool;
pub mod char;
pub mod float;
pub mod int;
pub mod object;
pub mod string;
pub mod vector;

pub mod array_m;
pub mod env_m;
pub mod fs_m;
pub mod io_m;
pub mod math_m;
pub mod net_m;
pub mod os_m;
pub mod typed_m;

pub const FOR_FUNC: &str = "next";

pub const INT_MODULE: &str = "int";
pub const FLOAT_MODULE: &str = "float";
pub const BOOL_MODULE: &str = "bool";
pub const CHAR_MODULE: &str = "char";
pub const STRING_MODULE: &str = "string";
pub const VECTOR_MODULE: &str = "vector";
pub const OBJECT_MODULE: &str = "object";
pub const TYPED_MODULE: &str = "typed";

pub fn globals() -> HashMap<String, Rc<RefCell<Value>>> {
    let mut globals = HashMap::new();
    set_field!(globals."print" = function!(_print));
    set_field!(globals."input" = function!(_input));
    set_field!(globals."assert" = function!(_assert));
    set_field!(globals."error" = function!(_error));
    set_field!(globals."exit" = function!(_exit));
    set_field!(globals."safe_call" = function!(_safe_call));
    set_field!(globals."type" = function!(_type));
    set_field!(globals."require" = function!(_require));
    set_field!(globals."raw_type" = function!(_raw_type));
    set_field!(globals."raw_get" = function!(_raw_get));
    set_field!(globals."raw_set" = function!(_raw_set));
    set_field!(globals."iter" = function!(_iter));
    set_field!(globals.FOR_FUNC = function!(_next));
    int::define(&mut globals);
    float::define(&mut globals);
    bool::define(&mut globals);
    char::define(&mut globals);
    string::define(&mut globals);
    vector::define(&mut globals);
    object::define(&mut globals);
    math_m::define(&mut globals);
    io_m::define(&mut globals);
    fs_m::define(&mut globals);
    env_m::define(&mut globals);
    net_m::define(&mut globals);
    os_m::define(&mut globals);
    typed_m::define(&mut globals);
    array_m::define(&mut globals);
    globals
}

pub fn _print(interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let args = args.into_iter().enumerate();
    let mut strings = vec![String::default(); args.len()];
    for (i, value) in args {
        strings.insert(
            i,
            value
                .call_tostring(interpreter)
                .map_err(|err| Into::<Box<dyn Error>>::into(err.to_string()))?,
        )
    }
    println!("{}", strings.join(" "));
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
pub fn _exit(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let code = typed!(args: Int?).unwrap_or_default() as i32;
    std::process::exit(code)
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
    Ok(Value::String(value.dynamic_typ()))
}
#[derive(Debug, Clone, PartialEq)]
pub struct RequireError {
    path: String,
    global_path: Option<String>,
}
impl Display for RequireError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "couldn't find any of these modules:\n\t{0}.luna\n\t{0}/mod.luna\n\t{1}/{0}.luna\n\t{1}/{0}/mod.luna",
            self.path, self.global_path.as_ref().unwrap_or(&"$LUNA_PATH".into())
        )
    }
}
impl Error for RequireError {}
pub const REQUIRE_SO_FUNC_NAME: &[u8] = b"luna_require";
pub fn _require(interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let path = typed!(args: String path => path.split('.').collect::<Vec<&str>>().join("/"));
    let (text, full_path) = if let Ok(text) = fs::read_to_string(format!("{path}.luna")) {
        (text, format!("{path}.luna"))
    } else if let Ok(text) = fs::read_to_string(format!("{path}/mod.luna")) {
        (text, format!("{path}/mod.luna"))
    } else if fs::File::open(format!("{path}.so")).is_ok() {
        unsafe {
            let lib = libloading::Library::new(format!("{path}.so"))?;
            let func: libloading::Symbol<
                unsafe extern fn(&mut Interpreter, Vec<Value>) -> Result<Value, Box<dyn Error>>,
            > = lib.get(REQUIRE_SO_FUNC_NAME)?;
            return func(interpreter, Vec::from_iter(args.map(|(_, v)| v)));
        }
    } else if let Some(global_path) = &interpreter.global_path {
        let current_path = env::current_dir().map_err(|_| "couldn't resolve current directory")?;
        let root_path = "/";
        let root_path = Path::new(&root_path);
        env::set_current_dir(root_path).map_err(|_| "couldn't change directory to root")?;
        if let Ok(text) = fs::read_to_string(format!("{global_path}/{path}.luna")) {
            env::set_current_dir(current_path).map_err(|_| "couldn't change directory")?;
            (text, format!("{global_path}/{path}.luna"))
        } else if let Ok(text) = fs::read_to_string(format!("{global_path}/{path}/mod.luna")) {
            env::set_current_dir(current_path).map_err(|_| "couldn't change directory")?;
            (text, format!("{global_path}/{path}/mod.luna"))
        } else if fs::File::open(format!("{global_path}/{path}.so")).is_ok() {
            unsafe {
                let lib = libloading::Library::new(format!("{global_path}/{path}.so"))?;
                let func: libloading::Symbol<
                    unsafe extern fn(
                        &mut Interpreter,
                        Vec<Value>,
                    ) -> Result<Value, Box<dyn Error>>,
                > = lib.get(REQUIRE_SO_FUNC_NAME)?;
                return func(interpreter, Vec::from_iter(args.map(|(_, v)| v)));
            }
        } else {
            env::set_current_dir(current_path).map_err(|_| "couldn't change directory")?;
            return Err(Box::new(RequireError {
                path,
                global_path: Some(global_path.clone()),
            }));
        }
    } else {
        return Err(Box::new(RequireError {
            path,
            global_path: None,
        }));
    };
    Ok(run_str::<Chunk>(&text, Some(&full_path))
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

pub fn _raw_type(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter();
    let v = args.next().unwrap_or_default();
    Ok(Value::String(v.typ().to_string()))
}
pub fn _raw_get(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let object = object.borrow();
    let key = typed!(args: String);
    Ok(object.get(&key).unwrap_or_default())
}
pub fn _raw_set(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let mut object = object.borrow_mut();
    let key = typed!(args: String);
    let value = args.next().unwrap_or_default().1;
    object.set(key, value);
    Ok(Value::default())
}

fn _iter(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    match value {
        Value::Vector(vector) => {
            let vector = vector.borrow();
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                IteratorObject(Box::new(vector.clone().into_iter())),
            )))))
        }
        Value::String(string) => Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
            IteratorObject(Box::new(
                string
                    .chars()
                    .map(Value::Char)
                    .collect::<Vec<Value>>()
                    .into_iter(),
            )),
        ))))),
        Value::Object(object) => {
            let object = object.borrow();
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                IteratorObject(Box::new(
                    object
                        .fields
                        .keys()
                        .cloned()
                        .map(Value::String)
                        .collect::<Vec<Value>>()
                        .into_iter(),
                )),
            )))))
        }
        value => Err(format!("cannot iterate over {}", value.dynamic_typ()).into()),
    }
}
fn _next(interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let iter = typed!(args);
    Ok(match &iter {
        Value::UserObject(object) => {
            let object = Rc::clone(object);
            let mut object = object.borrow_mut();
            object.call_mut("next", interpreter, vec![iter])?
        }
        Value::Object(object) => {
            let object: Rc<RefCell<Object>> = Rc::clone(object);
            let object = object.borrow();
            let value = object.get_meta(META_NEXT).unwrap_or_default();
            match value {
                Value::Function(kind) => match kind {
                    FunctionKind::Function(function) => {
                        interpreter.call(&function, vec![], None);
                        return Ok(interpreter
                            .run()
                            .map_err(|Located { value: err, pos: _ }| err)?
                            .unwrap_or_default());
                    }
                    FunctionKind::UserFunction(func) => func(interpreter, vec![])
                        .map_err(|err| RunTimeError::Custom(err.to_string()))?,
                },
                _ => Value::default(),
            }
        }
        Value::Function(kind) => match kind {
            FunctionKind::Function(function) => {
                interpreter.call(function, vec![], None);
                return Ok(interpreter
                    .run()
                    .map_err(|Located { value: err, pos: _ }| err)?
                    .unwrap_or_default());
            }
            FunctionKind::UserFunction(func) => {
                func(interpreter, vec![]).map_err(|err| RunTimeError::Custom(err.to_string()))?
            }
        },
        iter => return Err(format!("cannot iterate over {}", iter.dynamic_typ()).into()),
    })
}
fn _iter_next(
    interpreter: &mut Interpreter,
    mut args: Vec<Value>,
) -> Result<Value, Box<dyn Error>> {
    let Some(_self) = args.first().cloned() else {
        return Err(Box::new(UserObjectError::ExpectedSelf("null")));
    };
    args.remove(0);
    if let Value::UserObject(_self) = _self {
        let mut _self = _self.borrow_mut();
        _self.call_mut("next", interpreter, args)
    } else {
        Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
    }
}
fn _iter_collect(
    interpreter: &mut Interpreter,
    mut args: Vec<Value>,
) -> Result<Value, Box<dyn Error>> {
    let Some(_self) = args.first().cloned() else {
        return Err(Box::new(UserObjectError::ExpectedSelf("null")));
    };
    args.remove(0);
    if let Value::UserObject(_self) = _self {
        let mut _self = _self.borrow_mut();
        _self.call_mut("collect", interpreter, args)
    } else {
        Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
    }
}
pub trait CanBeIterator: Iterator<Item: Into<Value>> + Debug {
    fn call_next(&mut self) -> Result<Value, Box<dyn Error>> {
        Ok(self.next().map(|v| v.into()).unwrap_or_default())
    }
    fn call_collect(&mut self) -> Result<Value, Box<dyn Error>> {
        Ok(Value::Vector(Rc::new(RefCell::new(
            self.map(|v| v.into()).collect::<Vec<Value>>(),
        ))))
    }
    fn call_any(
        &mut self,
        interpreter: &mut Interpreter,
        func: FunctionKind,
    ) -> Result<Value, Box<dyn Error>> {
        match func {
            FunctionKind::Function(func) => {
                for v in self {
                    interpreter.call(&func, vec![v.into()], None);
                    let res = interpreter
                        .run()
                        .map_err(|err| Box::new(err.value))?
                        .unwrap_or_default();
                    if bool::from(res) {
                        return Ok(true.into());
                    }
                }
                Ok(false.into())
            }
            FunctionKind::UserFunction(func) => {
                for v in self {
                    let res = func(interpreter, vec![v.into()])?;
                    if bool::from(res) {
                        return Ok(true.into());
                    }
                }
                Ok(false.into())
            }
        }
    }
    fn call_all(
        &mut self,
        interpreter: &mut Interpreter,
        func: FunctionKind,
    ) -> Result<Value, Box<dyn Error>> {
        match func {
            FunctionKind::Function(func) => {
                for v in self {
                    interpreter.call(&func, vec![v.into()], None);
                    let res = interpreter
                        .run()
                        .map_err(|err| Box::new(err.value))?
                        .unwrap_or_default();
                    if !bool::from(res) {
                        return Ok(false.into());
                    }
                }
                Ok(true.into())
            }
            FunctionKind::UserFunction(func) => {
                for v in self {
                    let res = func(interpreter, vec![v.into()])?;
                    if !bool::from(res) {
                        return Ok(false.into());
                    }
                }
                Ok(true.into())
            }
        }
    }
}
impl CanBeIterator for std::vec::IntoIter<Value> {}
#[derive(Debug)]
pub struct IteratorObject(pub Box<dyn CanBeIterator<Item = Value>>);
userobject! {
    IteratorObject: "iterator";
    self
    mut (self, interpreter, args) {
        next : "next" {
            self.0.call_next()
        }
        any : "any" {
            let mut args = args.into_iter().enumerate();
            let func = typed!(args: Function);
            self.0.call_any(interpreter, func)
        }
        all : "all" {
            let mut args = args.into_iter().enumerate();
            let func = typed!(args: Function);
            self.0.call_all(interpreter, func)
        }
        collect : "collect" {
            self.0.call_collect()
        }
    }
}
