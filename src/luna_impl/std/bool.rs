use std::error::Error;

use crate::lang::{interpreter::Interpreter, value::Value};

pub fn _from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let (_, value) = args.next().unwrap_or_default();
    Ok(Value::Bool(value.into()))
}