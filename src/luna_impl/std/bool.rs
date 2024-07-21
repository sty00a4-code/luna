use crate::{
    function,
    lang::{interpreter::Interpreter, value::{Value, Object, FunctionKind}},
    luna_impl::std::BOOL_MODULE,
    object, set_field,
};
use std::{cell::RefCell, collections::HashMap, error::Error, rc::Rc};

pub fn define(globals: &mut HashMap<String, Rc<RefCell<Value>>>) {
    set_field!(
        globals.BOOL_MODULE = object! {
            "from" = function!(_from)
        }
    );
}

pub fn _from(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let (_, value) = args.next().unwrap_or_default();
    Ok(Value::Bool(value.into()))
}
