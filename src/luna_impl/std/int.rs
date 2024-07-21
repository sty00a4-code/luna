use crate::{
    lang::{interpreter::Interpreter, value::Value},
    typed, ExpectedType,
};
use std::{cell::RefCell, error::Error, rc::Rc};

pub fn _from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _from_bin(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    Ok(i64::from_str_radix(&string, 2)
        .map(Value::Int)
        .unwrap_or_default())
}
pub fn _from_hex(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let string = typed!(args: String);
    Ok(i64::from_str_radix(&string, 16)
        .map(Value::Int)
        .unwrap_or_default())
}
pub fn _bytes(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Int);
    Ok(Value::Vector(Rc::new(RefCell::new(
        value.to_be_bytes().into_iter().map(Value::from).collect(),
    ))))
}
