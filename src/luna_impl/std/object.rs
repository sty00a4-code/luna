use super::IteratorObject;
use crate::{
    function, lang::{
        interpreter::Interpreter,
        value::{FunctionKind, Object, Value, UserObject, UserObjectError},
    }, luna_impl::std::OBJECT_MODULE, object, option, set_field, typed, userobject, ExpectedType, ExpectedTypes
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
            "clear" = function!(_clear),
            "int" = function!(_int),
            "float" = function!(_float),
            "bool" = function!(_bool),
            "char" = function!(_char),
            "string" = function!(_string)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IntObject(i64);
userobject! {
    IntObject : "int";
    self
    yield {
        "value" = Value::Int(self.0)
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                IntObject(self.0),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            match key.as_str() {
                "value" => {
                    let value = typed!(args: Int);
                    self.0 = value;
                }
                _ => {}
            }
            Ok(Value::default())
        }
    }
}
pub fn _int(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Int);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        IntObject(value),
    )))))
}
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd,)]
pub struct FloatObject(f64);
userobject! {
    FloatObject : "float";
    self
    yield {
        "value" = Value::Float(self.0)
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                FloatObject(self.0),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            match key.as_str() {
                "value" => {
                    let value = typed!(args: Float);
                    self.0 = value;
                }
                _ => {}
            }
            Ok(Value::default())
        }
    }
}
pub fn _float(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Float);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        FloatObject(value),
    )))))
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BoolObject(bool);
userobject! {
    BoolObject : "bool";
    self
    yield {
        "value" = Value::Bool(self.0)
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                BoolObject(self.0),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            match key.as_str() {
                "value" => {
                    let value = typed!(args: Bool);
                    self.0 = value;
                }
                _ => {}
            }
            Ok(Value::default())
        }
    }
}
pub fn _bool(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Bool);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        BoolObject(value),
    )))))
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CharObject(char);
userobject! {
    CharObject : "char";
    self
    yield {
        "value" = Value::Char(self.0)
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                CharObject(self.0),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            match key.as_str() {
                "value" => {
                    let value = typed!(args: Char);
                    self.0 = value;
                }
                _ => {}
            }
            Ok(Value::default())
        }
    }
}
pub fn _char(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        CharObject(value),
    )))))
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StringObject(String);
userobject! {
    StringObject : "string";
    self
    yield {
        "value" = Value::String(self.0.clone())
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                StringObject(self.0.clone()),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            match key.as_str() {
                "value" => {
                    let value = typed!(args: String);
                    self.0 = value;
                }
                _ => {}
            }
            Ok(Value::default())
        }
    }
}
pub fn _string(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: String);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        StringObject(value),
    )))))
}