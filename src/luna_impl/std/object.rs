use super::IteratorObject;
use crate::{
    function,
    lang::{
        interpreter::Interpreter,
        value::{FunctionKind, Object, UserObject, UserObjectError, Value},
    },
    luna_impl::std::OBJECT_MODULE,
    object, option, set_field, typed, userobject, ExpectedType, ExpectedTypes,
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    error::Error,
    rc::Rc,
};

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
            "copy" = function!(_copy),
            "box" = function!(_box),
            "int" = function!(_int),
            "float" = function!(_float),
            "bool" = function!(_bool),
            "char" = function!(_char),
            "string" = function!(_string),
            "set" = function!(_set)
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
pub fn _copy(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let object = typed!(args: Object);
    let object = object.borrow();
    Ok(Value::Object(Rc::new(RefCell::new(Object::new(
        object.fields.clone(),
    )))))
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

#[derive(Debug, Clone, PartialEq)]
pub struct BoxObject(Value);
userobject! {
    BoxObject : "box";
    self
    yield {
        "value" = self.0.clone()
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                BoxObject(self.0.clone()),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            if key.as_str() == "value" {
                let value = typed!(args);
                self.0 = value;
            }
            Ok(Value::default())
        }
        __str : "__str" {
            Ok(Value::String(format!("box({:?})", self.0)))
        }
    }
}
pub fn _box(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        BoxObject(value),
    )))))
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IntBoxObject(i64);
userobject! {
    IntBoxObject : "int-box";
    self
    yield {
        "value" = Value::Int(self.0)
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                IntBoxObject(self.0),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            if key.as_str() == "value" {
                let value = typed!(args: Int);
                self.0 = value;
            }
            Ok(Value::default())
        }
        __str : "__str" {
            Ok(Value::String(format!("int-box({:?})", self.0)))
        }
    }
}
pub fn _int(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Int);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        IntBoxObject(value),
    )))))
}
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct FloatBoxObject(f64);
userobject! {
    FloatBoxObject : "float-box";
    self
    yield {
        "value" = Value::Float(self.0)
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                FloatBoxObject(self.0),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            if key.as_str() == "value" {
                let value = typed!(args: Float);
                self.0 = value;
            }
            Ok(Value::default())
        }
        __str : "__str" {
            Ok(Value::String(format!("float-box({:?})", self.0)))
        }
    }
}
pub fn _float(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Float);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        FloatBoxObject(value),
    )))))
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BoolBoxObject(bool);
userobject! {
    BoolBoxObject : "boolean-box";
    self
    yield {
        "value" = Value::Bool(self.0)
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                BoolBoxObject(self.0),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            if key.as_str() == "value" {
                let value = typed!(args: Bool);
                self.0 = value;
            }
            Ok(Value::default())
        }
        __str : "__str" {
            Ok(Value::String(format!("boolean-box({:?})", self.0)))
        }
    }
}
pub fn _bool(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Bool);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        BoolBoxObject(value),
    )))))
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CharBoxObject(char);
userobject! {
    CharBoxObject : "char-box";
    self
    yield {
        "value" = Value::Char(self.0)
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                CharBoxObject(self.0),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            if key.as_str() == "value" {
                let value = typed!(args: Char);
                self.0 = value;
            }
            Ok(Value::default())
        }
        __str : "__str" {
            Ok(Value::String(format!("char-box({:?})", self.0)))
        }
    }
}
pub fn _char(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: Char);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        CharBoxObject(value),
    )))))
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StringBoxObject(String);
userobject! {
    StringBoxObject : "string-box";
    self
    yield {
        "value" = Value::String(self.0.clone())
    }
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                StringBoxObject(self.0.clone()),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let key = typed!(args: String);
            if key.as_str() == "value" {
                let value = typed!(args: String);
                self.0 = value;
            }
            Ok(Value::default())
        }
        __str : "__str" {
            Ok(Value::String(format!("string-box({:?})", self.0)))
        }
    }
}
pub fn _string(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args: String);

    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        StringBoxObject(value),
    )))))
}
#[derive(Debug, Clone)]
pub struct SetObject(HashSet<Value>);
userobject! {
    SetObject : "set";
    self
    static (self, _i, _a) {
        clone : "clone" {
            Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                SetObject(self.0.clone()),
            )))))
        }
    }
    mut (self, _i, args) {
        __set : "__set" {
            let mut args = args.into_iter().enumerate();
            let value = typed!(args);
            let set = typed!(args: Bool);
            if set {
                self.0.insert(value);
            } else {
                self.0.remove(&value);
            }
            Ok(Value::default())
        }
        __get : "__get" {
            let mut args = args.into_iter().enumerate();
            let value = typed!(args);
            Ok(self.0.get(&value).cloned().unwrap_or_default())
        }
        __str : "__str" {
            Ok(Value::String(format!("{:?}", self.0)))
        }
    }
}
pub fn _set(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
        SetObject(HashSet::from_iter(args)),
    )))))
}
