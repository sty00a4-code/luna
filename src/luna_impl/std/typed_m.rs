use crate::{
    lang::{interpreter::Interpreter, value::Value},
    typed,
};
use std::error::Error;

pub fn _check(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    for (_, arg) in args {
        if let Value::String(typ) = arg {
            if value.dynamic_typ() == typ {
                return Ok(value);
            }
        }
    }
    Ok(Value::default())
}
pub fn _check_raw(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    for (_, arg) in args {
        if let Value::String(typ) = arg {
            if value.typ() == typ {
                return Ok(value);
            }
        }
    }
    Ok(Value::default())
}
pub fn _int(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value: Value = typed!(args);
    Ok(if value.typ() == "int" {
        value
    } else {
        Value::default()
    })
}
pub fn _float(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "float" {
        value
    } else {
        Value::default()
    })
}
pub fn _bool(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "bool" {
        value
    } else {
        Value::default()
    })
}
pub fn _char(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "char" {
        value
    } else {
        Value::default()
    })
}
pub fn _string(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "string" {
        value
    } else {
        Value::default()
    })
}
pub fn _vector(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "vector" {
        value
    } else {
        Value::default()
    })
}
pub fn _object(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.dynamic_typ() == "object" {
        value
    } else {
        Value::default()
    })
}
pub fn _object_raw(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "object" {
        value
    } else {
        Value::default()
    })
}
pub fn _function(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "function" {
        value
    } else {
        Value::default()
    })
}
pub fn _numeric(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() == "int" || value.typ() == "float" {
        value
    } else {
        Value::default()
    })
}
pub fn _iterable(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(
        if value.typ() == "string" || value.typ() == "vector" || value.typ() == "object" {
            value
        } else {
            Value::default()
        },
    )
}
pub fn _options(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    for (_, arg) in args {
        if value == arg {
            return Ok(value);
        }
    }
    Ok(Value::default())
}
pub fn _some(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let value = typed!(args);
    Ok(if value.typ() != "null" {
        value
    } else {
        Value::default()
    })
}
