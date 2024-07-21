use crate::{
    lang::{interpreter::Interpreter, value::Value},
    option,
    ExpectedTypes
};
use std::error::Error;

pub fn _abs(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Int(value.abs()))
        },
        Float => value {
            Ok(Value::Float(value.abs()))
        }
    )
}
pub fn _sqrt(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).sqrt()))
        },
        Float => value {
            Ok(Value::Float(value.sqrt()))
        }
    )
}
pub fn _exp(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).exp()))
        },
        Float => value {
            Ok(Value::Float(value.exp()))
        }
    )
}
pub fn _exp2(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).exp2()))
        },
        Float => value {
            Ok(Value::Float(value.exp2()))
        }
    )
}
pub fn _exp_m1(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).exp_m1()))
        },
        Float => value {
            Ok(Value::Float(value.exp_m1()))
        }
    )
}
pub fn _signum(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).signum()))
        },
        Float => value {
            Ok(Value::Float(value.signum()))
        }
    )
}
pub fn _fract(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).fract()))
        },
        Float => value {
            Ok(Value::Float(value.fract()))
        }
    )
}
pub fn _sin(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).sin()))
        },
        Float => value {
            Ok(Value::Float(value.sin()))
        }
    )
}
pub fn _cos(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).cos()))
        },
        Float => value {
            Ok(Value::Float(value.cos()))
        }
    )
}
pub fn _tan(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).tan()))
        },
        Float => value {
            Ok(Value::Float(value.tan()))
        }
    )
}
pub fn _sinh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).sinh()))
        },
        Float => value {
            Ok(Value::Float(value.sinh()))
        }
    )
}
pub fn _cosh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).cosh()))
        },
        Float => value {
            Ok(Value::Float(value.cosh()))
        }
    )
}
pub fn _tanh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).tanh()))
        },
        Float => value {
            Ok(Value::Float(value.tanh()))
        }
    )
}
pub fn _asin(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).asin()))
        },
        Float => value {
            Ok(Value::Float(value.asin()))
        }
    )
}
pub fn _acos(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).acos()))
        },
        Float => value {
            Ok(Value::Float(value.acos()))
        }
    )
}
pub fn _atan(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).atan()))
        },
        Float => value {
            Ok(Value::Float(value.atan()))
        }
    )
}
pub fn _asinh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).asinh()))
        },
        Float => value {
            Ok(Value::Float(value.asinh()))
        }
    )
}
pub fn _acosh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).acosh()))
        },
        Float => value {
            Ok(Value::Float(value.acosh()))
        }
    )
}
pub fn _atanh(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).atanh()))
        },
        Float => value {
            Ok(Value::Float(value.atanh()))
        }
    )
}
pub fn _deg(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).to_degrees()))
        },
        Float => value {
            Ok(Value::Float(value.to_degrees()))
        }
    )
}
pub fn _rad(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    option!(args:
        Int => value {
            Ok(Value::Float((value as f64).to_radians()))
        },
        Float => value {
            Ok(Value::Float(value.to_radians()))
        }
    )
}
pub fn _random(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::Float(rand::random()))
}
