use crate::{
    function,
    lang::{
        interpreter::Interpreter,
        value::{FunctionKind, Object, UserObject, UserObjectError, Value},
    },
    object, set_field, typed, userobject, ExpectedType,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    io::{self, Read, Stderr, Stdin, Stdout, Write},
    rc::Rc,
};

pub fn define(globals: &mut HashMap<String, Rc<RefCell<Value>>>) {
    set_field!(globals."io" = object! {
        "write" = function!(_write),
        "flush" = function!(_flush),
        "stdin" = function!(_stdin),
        "stdout" = function!(_stdout),
        "stderr" = function!(_stderr)
    });
}

pub fn _write(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let text = typed!(args: String);
    write!(io::stdout(), "{}", text)?;
    Ok(Value::default())
}
pub fn _flush(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    io::stdout().flush()?;
    Ok(Value::default())
}
pub fn _stdin(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        StdinObject(io::stdin()),
    )))))
}
pub fn _stdout(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        StdoutObject(io::stdout()),
    )))))
}
pub fn _stderr(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        StderrObject(io::stderr()),
    )))))
}

#[derive(Debug)]
pub struct StdinObject(Stdin);
userobject! {
    StdinObject: "stdin";
    self
    mut (self, _i, args) {
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
    mut (self, _i, args) {
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
    mut (self, _i, args) {
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
