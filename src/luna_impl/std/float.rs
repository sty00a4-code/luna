use std::{cell::RefCell, collections::HashMap, error::Error, rc::Rc};

use crate::{
    function,
    lang::{interpreter::Interpreter, value::{Value, Object, FunctionKind}},
    luna_impl::std::FLOAT_MODULE,
    object, set_field, typed, ExpectedType,
};

pub fn define(globals: &mut HashMap<String, Rc<RefCell<Value>>>) {
    set_field!(
        globals.FLOAT_MODULE = object! {
            "from" = function!(_from),
            "floor" = function!(_floor),
            "ceil" = function!(_ceil),
            "round" = function!(_round)
        }
    );
}

pub fn _from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _floor(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Float);
    Ok(Value::Float(value.floor()))
}
pub fn _ceil(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Float);
    Ok(Value::Float(value.ceil()))
}
pub fn _round(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Float);
    Ok(Value::Float(value.round()))
}
