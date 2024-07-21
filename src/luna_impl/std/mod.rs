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
    set_field!(
        globals.INT_MODULE = object! {
            "from" = function!(int::_from),
            "from_bin" = function!(int::_from_bin),
            "from_hex" = function!(int::_from_hex),
            "bytes" = function!(int::_bytes)
        }
    );
    set_field!(
        globals.FLOAT_MODULE = object! {
            "from" = function!(float::_from),
            "floor" = function!(float::_floor),
            "ceil" = function!(float::_ceil),
            "round" = function!(float::_round)
        }
    );
    set_field!(
        globals.BOOL_MODULE = object! {
            "from" = function!(bool::_from)
        }
    );
    set_field!(
        globals.CHAR_MODULE = object! {
            "from" = function!(char::_from),
            "byte" = function!(char::_byte),
            "is_whitespace" = function!(char::_is_whitespace),
            "is_alphabetic" = function!(char::_is_alphabetic),
            "is_alphanumeric" = function!(char::_is_alphanumeric),
            "is_control" = function!(char::_is_control),
            "is_digit" = function!(char::_is_digit),
            "is_graphic" = function!(char::_is_graphic),
            "is_hex" = function!(char::_is_hex),
            "is_lower" = function!(char::_is_lower),
            "is_upper" = function!(char::_is_upper)
        }
    );
    set_field!(globals."str" = function!(string::_from));
    set_field!(
        globals.STRING_MODULE = object! {
            "lowercase" = ('a'..='z').collect::<Vec<char>>(),
            "uppercase" = ('A'..='Z').collect::<Vec<char>>(),
            "letters" = ('a'..='z').chain('A'..='Z').collect::<Vec<char>>(),
            "from" = globals["str"].borrow().clone(),
            "len" = function!(string::_len),
            "iter" = function!(string::_iter),
            "get" = function!(string::_get),
            "sub" = function!(string::_sub),
            "split" = function!(string::_split),
            "split_amount" = function!(string::_split_amount),
            "split_at" = function!(string::_split_at),
            "split_off" = function!(string::_split_off),
            "rep" = function!(string::_rep),
            "rev" = function!(string::_rev),
            "find" = function!(string::_find),
            "format" = function!(string::_format),
            "bin" = function!(int::_from_bin),
            "hex" = function!(int::_from_hex),
            "start" = function!(string::_starts_with),
            "end" = function!(string::_ends_with)
        }
    );
    set_field!(
        globals.VECTOR_MODULE = object! {
            "iter" = function!(vector::_iter),
            "len" = function!(vector::_len),
            "get" = function!(vector::_get),
            "contains" = function!(vector::_contains),
            "pos" = function!(vector::_position),
            "push" = function!(vector::_push),
            "pop" = function!(vector::_pop),
            "insert" = function!(vector::_insert),
            "join" = function!(vector::_join),
            "swap" = function!(vector::_swap),
            "copy" = function!(vector::_copy),
            "clear" = function!(vector::_clear)
        }
    );
    set_field!(globals."keys" = function!(object::_keys));
    set_field!(globals."values" = function!(object::_values));
    set_field!(globals."setmeta" = function!(object::_setmeta));
    set_field!(globals."getmeta" = function!(object::_getmeta));
    set_field!(
        globals.OBJECT_MODULE = object! {
            "len" = function!(object::_len),
            "keys" = globals["keys"].borrow().clone(),
            "values" = globals["values"].borrow().clone(),
            "setmeta" = globals["setmeta"].borrow().clone(),
            "getmeta" = globals["getmeta"].borrow().clone(),
            "clear" = function!(object::_clear)
        }
    );
    set_field!(globals."range" = function!(object::_range));
    set_field!(globals."math" = object! {
        "pi" = Value::Float(std::f64::consts::PI),
        "nan" = Value::Float(f64::NAN),
        "inf" = Value::Float(f64::INFINITY),
        "e" = Value::Float(f64::EPSILON),
        "abs" = function!(math_m::_abs),
        "sqrt" = function!(math_m::_sqrt),
        "exp" = function!(math_m::_exp),
        "exp2" = function!(math_m::_exp2),
        "exp_m1" = function!(math_m::_exp_m1),
        "signum" = function!(math_m::_signum),
        "fract" = function!(math_m::_fract),
        "cos" = function!(math_m::_cos),
        "sin" = function!(math_m::_sin),
        "tan" = function!(math_m::_tan),
        "cosh" = function!(math_m::_cosh),
        "sinh" = function!(math_m::_sinh),
        "tanh" = function!(math_m::_tanh),
        "acos" = function!(math_m::_acos),
        "asin" = function!(math_m::_asin),
        "atan" = function!(math_m::_atan),
        "acosh" = function!(math_m::_acosh),
        "asinh" = function!(math_m::_asinh),
        "atanh" = function!(math_m::_atanh),
        "deg" = function!(math_m::_deg),
        "rad" = function!(math_m::_rad),
        "random" = function!(math_m::_random)
    });
    set_field!(globals."io" = object! {
        "write" = function!(io_m::_write),
        "flush" = function!(io_m::_flush),
        "stdin" = function!(io_m::_stdin),
        "stdout" = function!(io_m::_stdout),
        "stderr" = function!(io_m::_stderr)
    });
    set_field!(globals."fs" = object! {
        "open" = function!(fs_m::_open),
        "list" = function!(fs_m::_list),
        "type" = function!(fs_m::_type)
    });
    set_field!(globals."env" = object! {
        "var" = function!(env_m::_var),
        "set_var" = function!(env_m::_set_var),
        "remove_var" = function!(env_m::_remove_var),
        "vars" = function!(env_m::_vars),
        "current_dir" = function!(env_m::_current_dir),
        "current_exe" = function!(env_m::_current_exe),
        "set_current_dir" = function!(env_m::_set_current_dir),
        "args" = function!(env_m::_args)
    });
    set_field!(globals."net" = object! {
        "bind" = function!(net_m::_bind),
        "connect" = function!(net_m::_connect)
    });
    set_field!(globals."os" = object! {
        "exec" = function!(os_m::_exec),
        "time" = function!(os_m::_time),
        "sleep" = function!(os_m::_sleep)
    });
    set_field!(globals."options" = function!(typed_m::_options));
    set_field!(globals."some" = function!(typed_m::_some));
    set_field!(globals."typed" = object! {
        "type" = globals["type"].borrow().clone(),
        "raw_type" = globals["raw_type"].borrow().clone(),
        "check" =  function!(typed_m::_check),
        "check_raw" =  function!(typed_m::_check_raw),
        "int" = function!(typed_m::_int),
        "float" = function!(typed_m::_float),
        "bool" = function!(typed_m::_bool),
        "char" = function!(typed_m::_char),
        "string" = function!(typed_m::_string),
        "vector" = function!(typed_m::_vector),
        "object" = function!(typed_m::_object),
        "object_raw" = function!(typed_m::_object_raw),
        "function" = function!(typed_m::_function),
        "numeric" = function!(typed_m::_numeric),
        "iterable" = function!(typed_m::_iterable),
        "options" = globals["options"].borrow().clone(),
        "some" = globals["some"].borrow().clone()
    });
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
                unsafe extern "C" fn(&mut Interpreter, Vec<Value>) -> Result<Value, Box<dyn Error>>,
            > = lib.get(b"require")?;
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
                    unsafe extern "C" fn(
                        &mut Interpreter,
                        Vec<Value>,
                    ) -> Result<Value, Box<dyn Error>>,
                > = lib.get(b"require")?;
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
pub trait CanBeIterator: Iterator<Item = Value> + Debug {
    fn call_next(&mut self) -> Result<Value, Box<dyn Error>> {
        Ok(self.next().unwrap_or_default())
    }
    fn call_collect(&mut self) -> Result<Value, Box<dyn Error>> {
        Ok(Value::Vector(Rc::new(RefCell::new(
            self.collect::<Vec<Value>>(),
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
                    interpreter.call(&func, vec![v], None);
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
                    let res = func(interpreter, vec![v])?;
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
                    interpreter.call(&func, vec![v], None);
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
                    let res = func(interpreter, vec![v])?;
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
pub struct IteratorObject(pub Box<dyn CanBeIterator>);
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
