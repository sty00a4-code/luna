use super::IteratorObject;
use crate::{
    function,
    lang::{
        interpreter::Interpreter,
        value::{FunctionKind, Object, Value},
    },
    luna_impl::std::OBJECT_MODULE,
    object, option, set_field, typed, ExpectedType, ExpectedTypes,
};
use std::{cell::RefCell, collections::HashMap, error::Error, rc::Rc};

pub fn define(globals: &mut HashMap<String, Rc<RefCell<Value>>>) {
    set_field!(globals."keys" = function!(_keys));
    set_field!(globals."values" = function!(_values));
    set_field!(globals."setmeta" = function!(_setmeta));
    set_field!(globals."getmeta" = function!(_getmeta));
    set_field!(
        globals.OBJECT_MODULE = object! {
            "len" = function!(_len),
            "keys" = globals["keys"].borrow().clone(),
            "values" = globals["values"].borrow().clone(),
            "setmeta" = globals["setmeta"].borrow().clone(),
            "getmeta" = globals["getmeta"].borrow().clone(),
            "clear" = function!(_clear)
        }
    );
    set_field!(globals."range" = function!(_range));
}

pub fn _len(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let object = object.borrow();

    Ok(Value::Int(object.fields.len() as i64))
}
pub fn _keys(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _values(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _setmeta(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _getmeta(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
pub fn _clear(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
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
