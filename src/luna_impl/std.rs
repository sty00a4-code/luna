#![allow(unused_macros)]
use crate::{
    function,
    lang::{
        interpreter::{Interpreter, RunTimeError},
        value::{FunctionKind, Object, UserObject, UserObjectError, Value, META_NEXT},
    },
    luna_impl::ast::Chunk,
    object, option, run_str, set_field, typed, userobject, ExpectedType, ExpectedTypes,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    env,
    error::Error,
    fmt::{Debug, Display},
    fs::{self, File},
    io::{self, Read, Stderr, Stdin, Stdout, Write},
    net::{TcpListener, TcpStream},
    path::Path,
    process::Command,
    rc::Rc,
    thread,
    time::Duration,
};

use super::position::Located;

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
    set_field!(globals."next" = function!(_next));
    set_field!(
        globals.INT_MODULE = object! {
            "from" = function!(_int_from),
            "from_bin" = function!(_int_from_bin),
            "from_hex" = function!(_int_from_hex),
            "bytes" = function!(_int_bytes)
        }
    );
    set_field!(
        globals.FLOAT_MODULE = object! {
            "from" = function!(_float_from),
            "floor" = function!(_float_floor),
            "ceil" = function!(_float_ceil),
            "round" = function!(_float_round)
        }
    );
    set_field!(
        globals.BOOL_MODULE = object! {
            "from" = function!(_bool_from)
        }
    );
    set_field!(
        globals.CHAR_MODULE = object! {
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
        }
    );
    set_field!(globals."str" = function!(_string_from));
    set_field!(
        globals.STRING_MODULE = object! {
            "lowercase" = ('a'..='z').collect::<Vec<char>>(),
            "uppercase" = ('A'..='Z').collect::<Vec<char>>(),
            "letters" = ('a'..='z').chain('A'..='Z').collect::<Vec<char>>(),
            "from" = globals["str"].borrow().clone(),
            "len" = function!(_string_len),
            "iter" = function!(_string_iter),
            "get" = function!(_string_get),
            "sub" = function!(_string_sub),
            "sep" = function!(_string_sep),
            "rep" = function!(_string_rep),
            "rev" = function!(_string_rev),
            "find" = function!(_string_find),
            "format" = function!(_string_format),
            "bin" = function!(_int_from_bin),
            "hex" = function!(_int_from_hex)
        }
    );
    set_field!(
        globals.VECTOR_MODULE = object! {
            "iter" = function!(_vector_iter),
            "len" = function!(_vector_len),
            "get" = function!(_vector_get),
            "contains" = function!(_vector_contains),
            "pos" = function!(_vector_position),
            "push" = function!(_vector_push),
            "pop" = function!(_vector_pop),
            "insert" = function!(_vector_insert),
            "join" = function!(_vector_join),
            "swap" = function!(_vector_swap),
            "copy" = function!(_vector_copy),
            "clear" = function!(_vector_clear)
        }
    );
    set_field!(globals."keys" = function!(_object_keys));
    set_field!(globals."values" = function!(_object_values));
    set_field!(globals."setmeta" = function!(_object_setmeta));
    set_field!(globals."getmeta" = function!(_object_getmeta));
    set_field!(
        globals.OBJECT_MODULE = object! {
            "len" = function!(_object_len),
            "keys" = globals["keys"].borrow().clone(),
            "values" = globals["values"].borrow().clone(),
            "setmeta" = globals["setmeta"].borrow().clone(),
            "getmeta" = globals["getmeta"].borrow().clone(),
            "clear" = function!(_object_clear)
        }
    );
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
    set_field!(globals."io" = object! {
        "write" = function!(_io_write),
        "flush" = function!(_io_flush),
        "stdin" = function!(_io_stdin),
        "stdout" = function!(_io_stdout),
        "stderr" = function!(_io_stderr)
    });
    set_field!(globals."fs" = object! {
        "open" = function!(_fs_open),
        "list" = function!(_fs_list),
        "type" = function!(_fs_type)
    });
    set_field!(globals."env" = object! {
        "var" = function!(_env_var),
        "set_var" = function!(_env_set_var),
        "remove_var" = function!(_env_remove_var),
        "vars" = function!(_env_vars),
        "current_dir" = function!(_env_current_dir),
        "current_exe" = function!(_env_current_exe),
        "set_current_dir" = function!(_env_set_current_dir),
        "args" = function!(_env_args)
    });
    set_field!(globals."net" = object! {
        "bind" = function!(_net_bind),
        "connect" = function!(_net_connect)
    });
    set_field!(globals."os" = object! {
        "exec" = function!(_os_exec),
        "time" = function!(_os_time),
        "sleep" = function!(_os_sleep)
    });
    set_field!(globals."options" = function!(_typed_options));
    set_field!(globals."some" = function!(_typed_some));
    set_field!(globals."typed" = object! {
        "type" = globals["type"].borrow().clone(),
        "check" =  function!(_typed_check),
        "check_raw" =  function!(_typed_check_raw),
        "int" = function!(_typed_int),
        "float" = function!(_typed_float),
        "bool" = function!(_typed_bool),
        "char" = function!(_typed_char),
        "string" = function!(_typed_string),
        "vector" = function!(_typed_vector),
        "object" = function!(_typed_object),
        "object_raw" = function!(_typed_object_raw),
        "function" = function!(_typed_function),
        "numeric" = function!(_typed_numeric),
        "iterable" = function!(_typed_iterable),
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
pub fn _int_from_bin(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    Ok(i64::from_str_radix(&string, 2)
        .map(Value::Int)
        .unwrap_or_default())
}
pub fn _int_from_hex(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    Ok(i64::from_str_radix(&string, 16)
        .map(Value::Int)
        .unwrap_or_default())
}
pub fn _int_bytes(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Int);
    Ok(Value::Vector(Rc::new(RefCell::new(
        value.to_be_bytes().into_iter().map(Value::from).collect(),
    ))))
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

pub fn _string_from(
    interpreter: &mut Interpreter,
    args: Vec<Value>,
) -> Result<Value, Box<dyn Error>> {
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
pub fn _string_iter(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _string_len(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);

    Ok(Value::Int(string.len() as i64))
}
pub fn _string_get(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _string_format(
    interpreter: &mut Interpreter,
    args: Vec<Value>,
) -> Result<Value, Box<dyn Error>> {
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

pub fn _vector_iter(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let vector = vector.borrow();

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        IteratorObject(Box::new(vector.clone().into_iter())),
    )))))
}
pub fn _vector_len(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let vector = vector.borrow();

    Ok(Value::Int(vector.len() as i64))
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
pub fn _vector_position(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let vector = vector.borrow();
    let value = args.next().map(|(_, v)| v).unwrap_or_default();

    Ok(vector
        .iter()
        .position(|v| v == &value)
        .map(|pos| pos.into())
        .unwrap_or_default())
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
pub fn _vector_insert(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let mut vector = vector.borrow_mut();
    let index = typed!(args: Int int => int.unsigned_abs() as usize);
    let value = args.next().map(|(_, v)| v).unwrap_or_default();
    if index <= vector.len() {
        vector.insert(index, value);
    }
    Ok(Value::default())
}
pub fn _vector_join(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let vector = vector.borrow();
    let sep = typed!(args: String);

    Ok(Value::String(
        vector
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<String>>()
            .join(&sep),
    ))
}
pub fn _vector_swap(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let mut vector = vector.borrow_mut();
    let index1 = typed!(args: Int int => int.unsigned_abs() as usize);
    let index2 = typed!(args: Int int => int.unsigned_abs() as usize);
    if vector.get(index1).is_some() && vector.get(index2).is_some() {
        vector.swap(index1, index2);
    }
    Ok(Value::default())
}
pub fn _vector_copy(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let vector = vector.borrow();

    Ok(vector.clone().into())
}
pub fn _vector_clear(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let vector = typed!(args: Vector);
    let mut vector = vector.borrow_mut();
    vector.clear();
    Ok(Value::default())
}

pub fn _object_len(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let object = object.borrow();

    Ok(Value::Int(object.fields.len() as i64))
}
pub fn _object_keys(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
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
pub fn _object_values(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let object = object.borrow();

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        IteratorObject(Box::new(
            object
                .fields
                .values()
                .cloned()
                .collect::<Vec<Value>>()
                .into_iter(),
        )),
    )))))
}
pub fn _object_setmeta(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Object => object {
            {
                let mut object = object.borrow_mut();
                let meta = typed!(args: Object?);

                object.meta = meta;
            }
            Ok(Value::Object(object))
        },
        Function => kind {
            if let FunctionKind::Function(func) = kind {
                let meta = typed!(args: Object?);
                func.borrow_mut().meta = meta;
                Ok(Value::Function(FunctionKind::Function(func)))
            } else {
                Ok(Value::default())
            }
        }
    )
}
pub fn _object_getmeta(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Object => object {
            let object = object.borrow();
            Ok(object
                .meta
                .as_ref()
                .map(|o| Value::Object(Rc::clone(o)))
                .unwrap_or_default())
        },
        Function => kind {
            if let FunctionKind::Function(func) = kind {
                let func = func.borrow();
                Ok(func
                    .meta
                    .as_ref()
                    .map(|o| Value::Object(Rc::clone(o)))
                    .unwrap_or_default())
            } else {
                Ok(Value::default())
            }
        }
    )
}
pub fn _object_clear(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let mut object = object.borrow_mut();
    object.fields.clear();
    Ok(Value::default())
}
pub fn _range(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let start = typed!(args: Int);
    let end = typed!(args: Int?);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        IteratorObject(Box::new(
            if let Some(end) = end {
                start..end
            } else {
                0..start
            }
            .map(Value::Int)
            .collect::<Vec<Value>>()
            .into_iter(),
        )),
    )))))
}

pub fn _typed_check(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    for (_, arg) in args {
        if let Value::String(typ) = arg {
            if value.dynamic_typ() == typ {
                return Ok(value);
            }
        }
    }
    Ok(Value::default())
}
pub fn _typed_check_raw(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    for (_, arg) in args {
        if let Value::String(typ) = arg {
            if value.typ() == typ {
                return Ok(value);
            }
        }
    }
    Ok(Value::default())
}
pub fn _typed_int(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value: Value = typed!(args);
    Ok(if value.typ() == "int" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_float(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "float" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_bool(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "bool" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_char(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "char" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_string(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "string" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_vector(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "vector" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_object(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.dynamic_typ() == "object" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_object_raw(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "object" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_function(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "function" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_numeric(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "int" || value.typ() == "float" {
        value
    } else {
        Value::default()
    })
}
pub fn _typed_iterable(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(
        if value.typ() == "string" || value.typ() == "vector" || value.typ() == "object" {
            value
        } else {
            Value::default()
        },
    )
}
pub fn _typed_options(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    for (_, arg) in args {
        if value == arg {
            return Ok(value);
        }
    }
    Ok(Value::default())
}
pub fn _typed_some(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() != "null" {
        value
    } else {
        Value::default()
    })
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

pub fn _io_write(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let text = typed!(args: String);
    write!(io::stdout(), "{}", text)?;
    Ok(Value::default())
}
pub fn _io_flush(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    io::stdout().flush()?;
    Ok(Value::default())
}
pub fn _io_stdin(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        StdinObject(io::stdin()),
    )))))
}
pub fn _io_stdout(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        StdoutObject(io::stdout()),
    )))))
}
pub fn _io_stderr(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        StderrObject(io::stderr()),
    )))))
}

#[derive(Debug, Clone, PartialEq)]
pub struct InvalidOptionError(String);
impl Display for InvalidOptionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid option {:?}", self.0)
    }
}
impl Error for InvalidOptionError {}
pub fn _fs_open(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let path = typed!(args: String);
    let (read, write, append) = typed!(args: String options => match options.as_str() {
        "w" => (true, true, false),
        "r" => (true, false, false),
        "a" => (true, false, true),
        _ => return Err(InvalidOptionError(options).into())
    });
    Ok(File::options()
        .read(read)
        .write(write)
        .create(write)
        .append(append)
        .open(path)
        .map(|file| Value::UserObject(Rc::new(RefCell::new(Box::new(FileObject(file))))))
        .unwrap_or_default())
}
pub fn _fs_list(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let path = typed!(args: String);
    Ok(fs::read_dir(path)?
        .flatten()
        .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
        .collect::<Vec<String>>()
        .into())
}
pub fn _fs_type(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let path = typed!(args: String);
    if fs::metadata(&path)?.is_dir() {
        Ok(Value::String("dir".to_string()))
    } else if fs::metadata(&path)?.is_file() {
        Ok(Value::String("file".to_string()))
    } else {
        Ok(Value::default())
    }
}

pub fn _env_set_var(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let var = typed!(args: String);
    let value = args.next().unwrap_or_default().1.to_string();
    env::set_var(var, value);
    Ok(Value::default())
}
pub fn _env_var(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let var = typed!(args: String);
    Ok(env::var(var).map(Value::String).unwrap_or_default())
}
pub fn _env_remove_var(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let var = typed!(args: String);
    env::remove_var(var);
    Ok(Value::default())
}
pub fn _env_vars(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::Object(Rc::new(RefCell::new(Object::new(
        env::vars().map(|(k, v)| (k, Value::String(v))).collect(),
    )))))
}
pub fn _env_current_dir(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(env::current_dir()?
        .to_str()
        .map(|s| Value::String(s.to_string()))
        .unwrap_or_default())
}
pub fn _env_current_exe(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(env::current_exe()?
        .to_str()
        .map(|s| Value::String(s.to_string()))
        .unwrap_or_default())
}
pub fn _env_set_current_dir(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::Object(Rc::new(RefCell::new(Object::new(
        env::vars().map(|(k, v)| (k, Value::String(v))).collect(),
    )))))
}
pub fn _env_args(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(env::args().collect::<Vec<String>>().into())
}

pub fn _net_bind(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let addr = typed!(args: String);
    let port = typed!(args: Int port => u16::try_from(port))
        .map_err(|_| Into::<Box<dyn Error>>::into("invalid port"))?;
    Ok(TcpListener::bind((addr, port))
        .map(|listener| {
            Value::UserObject(Rc::new(RefCell::new(Box::new(TcpListenerObject(listener)))))
        })
        .unwrap_or_default())
}
pub fn _net_connect(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let addr = typed!(args: String);
    let port = typed!(args: Int port => u16::try_from(port))
        .map_err(|_| Into::<Box<dyn Error>>::into("invalid port"))?;
    Ok(TcpStream::connect((addr, port))
        .map(|stream| Value::UserObject(Rc::new(RefCell::new(Box::new(TcpStreamObject(stream))))))
        .unwrap_or_default())
}

pub fn _os_exec(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let command = typed!(args: String);
    let args = args.map(|(_, v)| v.to_string()).collect::<Vec<String>>();
    let output = Command::new(command).args(args).output()?;
    Ok(object!(
        "ok" = output
            .stdout
            .into_iter()
            .map(|b| b as char)
            .collect::<String>(),
        "err" = output
            .stderr
            .into_iter()
            .map(|b| b as char)
            .collect::<String>()
    ))
}
pub fn _os_time(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::Float(
        chrono::Utc::now().timestamp_micros() as f64 / 1_000_000.,
    ))
}
pub fn _os_sleep(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let secs = option!(args: Float => secs { secs }, Int => secs { secs as f64 });
    thread::sleep(Duration::from_secs_f64(secs));
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
            object.call_mut("next", vec![iter])?
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
                interpreter.call(&function, vec![], None);
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
fn _iter_next(_: &mut Interpreter, mut args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let Some(_self) = args.first().cloned() else {
        return Err(Box::new(UserObjectError::ExpectedSelf("null")));
    };
    args.remove(0);
    if let Value::UserObject(_self) = _self {
        let mut _self = _self.borrow_mut();
        _self.call_mut("next", args)
    } else {
        Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
    }
}
fn _iter_collect(_: &mut Interpreter, mut args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let Some(_self) = args.first().cloned() else {
        return Err(Box::new(UserObjectError::ExpectedSelf("null")));
    };
    args.remove(0);
    if let Value::UserObject(_self) = _self {
        let mut _self = _self.borrow_mut();
        _self.call_mut("collect", args)
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
}
impl CanBeIterator for std::vec::IntoIter<Value> {}
#[derive(Debug)]
pub struct IteratorObject(pub Box<dyn CanBeIterator>);
userobject! {
    IteratorObject: "iterator";
    self
    mut (self, args) {
        next : "next" {
            self.0.call_next()
        }
        collect : "collect" {
            self.0.call_collect()
        }
    }
}

#[derive(Debug)]
pub struct StdinObject(Stdin);
userobject! {
    StdinObject: "stdin";
    self
    mut (self, args) {
        read : "read" {
            let mut args = args.into_iter().enumerate();
            let mode = typed!(args: String);
            let mut string = String::new();
            match mode.as_str() {
                "a" => self.0.read_to_string(&mut string)?,
                "l" => self.0.read_line(&mut string)?,
                _ => return Err(format!("invalid mode {mode:?} (expected: 'a'/'l')").into())
            };
            Ok(Value::String(string))
        }
    }
}
#[derive(Debug)]
pub struct StdoutObject(Stdout);
userobject! {
    StdoutObject: "stdout";
    self
    mut (self, args) {
        write : "write" {
            let mut args = args.into_iter().enumerate();
            let buf = typed!(args: String);
            Ok(Value::Int(self.0.write(&buf.into_bytes())? as i64))
        }
        flush : "flush" {
            self.0.flush()?;
            Ok(Value::default())
        }
    }
}
#[derive(Debug)]
pub struct StderrObject(Stderr);
userobject! {
    StderrObject: "stderr";
    self
    mut (self, args) {
        write : "write" {
            let mut args = args.into_iter().enumerate();
            let buf = typed!(args: String);
            Ok(Value::Int(self.0.write(&buf.into_bytes())? as i64))
        }
    }
}

#[derive(Debug)]
pub struct FileObject(File);
userobject! {
    FileObject: "file";
    self
    mut (self, args) {
        read : "read" {
            let mut string = String::new();
            self.0.read_to_string(&mut string)?;
            Ok(Value::String(string))
        }
        write : "write"  {
            let mut args = args.into_iter().enumerate();
            let buf = typed!(args: String);
            Ok(Value::Int(self.0.write(&buf.into_bytes())? as i64))
        }
    }
}

#[derive(Debug)]
pub struct TcpListenerObject(TcpListener);
userobject! {
    TcpListenerObject: "tcp-listener";
    self
    static (self, args) {
        addr : "addr"  {
            Ok(self
                .0
                .local_addr()
                .map(|addr| Value::String(addr.to_string()))
                .unwrap_or_default())
        }
    }
    mut (self, args) {
        accept : "accept" {
            Ok(self
                .0
                .accept()
                .map(|(stream, _)| {
                    Value::UserObject(Rc::new(RefCell::new(Box::new(TcpStreamObject(stream)))))
                })
                .unwrap_or_default())
        }
    }
}
#[derive(Debug)]
pub struct TcpStreamObject(TcpStream);
userobject! {
    TcpStreamObject: "tcp-stream";
    self
    static (self, _args) {
        local_addr : "local_addr" {
            Ok(self
                .0
                .local_addr()
                .map(|addr| Value::String(addr.to_string()))
                .unwrap_or_default())
        }
        peer_addr : "peer_addr" {
            Ok(self
                .0
                .peer_addr()
                .map(|addr| Value::String(addr.to_string()))
                .unwrap_or_default())
        }
    }
    mut (self, args) {
        read : "read" {
            let mut buf = String::new();
            let Ok(_) = self.0.read_to_string(&mut buf) else {
                return Ok(Value::default());
            };
            Ok(Value::String(buf))
        }
        write : "write"  {
            let mut args = args.into_iter().enumerate();
            let message = typed!(args: String);
            Ok(self.0.write(message.as_bytes()).unwrap_or_default().into())
        }
        flush : "flush"  {
            self.0.flush()?;
            Ok(Value::default())
        }
    }
}
