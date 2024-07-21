use std::time::Duration;

use crate::{
    lang::value::Object,
    lang::{interpreter::Interpreter, value::Value},
    option, typed, ExpectedType, ExpectedTypes,
};
use std::{cell::RefCell, collections::HashMap, error::Error, process::Command, rc::Rc, thread};

use super::object;

pub fn _exec(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _time(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::Float(
        chrono::Utc::now().timestamp_micros() as f64 / 1_000_000.,
    ))
}
pub fn _sleep(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let secs = option!(args: Float => secs { secs }, Int => secs { secs as f64 });
    thread::sleep(Duration::from_secs_f64(secs));
    Ok(Value::default())
}
