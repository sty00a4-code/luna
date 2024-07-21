use crate::{
    function,
    lang::{
        interpreter::Interpreter,
        value::{Object, Value, FunctionKind},
    },
    object, set_field, typed, ExpectedType,
};
use std::{cell::RefCell, collections::HashMap, env, error::Error, rc::Rc};

pub fn define(globals: &mut HashMap<String, Rc<RefCell<Value>>>) {
    set_field!(globals."env" = object! {
        "var" = function!(_var),
        "set_var" = function!(_set_var),
        "remove_var" = function!(_remove_var),
        "vars" = function!(_vars),
        "current_dir" = function!(_current_dir),
        "current_exe" = function!(_current_exe),
        "set_current_dir" = function!(_set_current_dir),
        "args" = function!(_args)
    });
}

pub fn _set_var(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let var = typed!(args: String);
    let value = args.next().unwrap_or_default().1.to_string();
    env::set_var(var, value);
    Ok(Value::default())
}
pub fn _var(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let var = typed!(args: String);
    Ok(env::var(var).map(Value::String).unwrap_or_default())
}
pub fn _remove_var(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let var = typed!(args: String);
    env::remove_var(var);
    Ok(Value::default())
}
pub fn _vars(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::Object(Rc::new(RefCell::new(Object::new(
        env::vars().map(|(k, v)| (k, Value::String(v))).collect(),
    )))))
}
pub fn _current_dir(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(env::current_dir()?
        .to_str()
        .map(|s| Value::String(s.to_string()))
        .unwrap_or_default())
}
pub fn _current_exe(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(env::current_exe()?
        .to_str()
        .map(|s| Value::String(s.to_string()))
        .unwrap_or_default())
}
pub fn _set_current_dir(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::Object(Rc::new(RefCell::new(Object::new(
        env::vars().map(|(k, v)| (k, Value::String(v))).collect(),
    )))))
}
pub fn _args(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(env::args().collect::<Vec<String>>().into())
}
