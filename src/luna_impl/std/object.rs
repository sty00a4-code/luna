use crate::{
    lang::{
        interpreter::Interpreter,
        value::{FunctionKind, Value},
    },
    option, typed, ExpectedType, ExpectedTypes
};
use std::{cell::RefCell, error::Error, rc::Rc};

use super::IteratorObject;

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
