use super::position::Located;
use crate::lang::{
    code::{BinaryOperation, ByteCode, Location, Source, UnaryOperation},
    value::{Function, FunctionKind, Object, Value},
};
use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, rc::Rc};

#[derive(Debug, Clone, Default)]
pub struct Interpreter {
    pub call_frames: Vec<CallFrame>,
    pub globals: Rc<RefCell<HashMap<String, Rc<RefCell<Value>>>>>,
}
#[derive(Debug, Clone)]
pub struct CallFrame {
    pub function: Rc<Function>,
    pub stack: Vec<Rc<RefCell<Value>>>,
    pub idx: usize,
    pub dst: Option<Location>,
    pub globals: Rc<RefCell<HashMap<String, Rc<RefCell<Value>>>>>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum RunTimeError {
    CannotCall(&'static str),
    CannotFieldInto {
        head: &'static str,
        field: &'static str,
    },
    CannotField(&'static str),
    InvalidBinary {
        op: BinaryOperation,
        left: &'static str,
        right: &'static str,
    },
    InvalidUnary {
        op: UnaryOperation,
        right: &'static str,
    },
    Custom(String),
}

impl Interpreter {
    pub fn call(&mut self, function: &Rc<Function>, dst: Option<Location>) {
        let mut stack = vec![];
        for _ in 0..function.closure.borrow().registers {
            stack.push(Rc::new(RefCell::new(Value::default())));
        }
        self.call_frames.push(CallFrame {
            function: Rc::clone(function),
            stack,
            idx: 0,
            dst,
            globals: Rc::clone(&self.globals)
        });
    }
    pub fn return_call(&mut self, src: Option<Source>) -> Option<Value> {
        let top_frame = self.call_frames.pop().expect("no frame on stack");
        if let Some(prev_frame) = self.call_frames.last_mut() {
            if let Some(dst) = prev_frame.dst {
                let value = top_frame
                    .source(&src.unwrap_or_default())
                    .expect("source not found");
                let dst = prev_frame.location(&dst).expect("location not found");
                *dst.borrow_mut() = value;
            }
        } else if let Some(src) = src {
            return Some(top_frame.source(&src).expect("source not found"));
        }
        None
    }
    pub fn step(&mut self) -> Result<Option<Value>, Located<RunTimeError>> {
        let frame = self.call_frames.last_mut().expect("no call frame");
        let Located {
            value: bytecode,
            pos,
        } = frame
            .function
            .closure
            .borrow()
            .code
            .get(frame.idx)
            .cloned()
            .expect("idx out of range");
        frame.idx += 1;
        match bytecode {
            ByteCode::None => {}
            ByteCode::Jump { addr } => {
                frame.idx = addr;
            }
            ByteCode::JumpIf {
                negative,
                cond,
                addr,
            } => {
                if frame.source(&cond).expect("cond not found").into() && !negative {
                    frame.idx = addr;
                }
            }
            ByteCode::JumpNull {
                negative,
                cond,
                addr,
            } => {
                if frame.source(&cond).expect("cond not found") == Value::default() && !negative {
                    frame.idx = addr;
                }
            }
            ByteCode::Call {
                dst,
                func,
                offset,
                amount,
            } => {
                let func = frame.source(&func).expect("func not found");
                let mut args = vec![];
                for register in offset..offset + amount {
                    args.push(
                        frame
                            .register(register)
                            .expect("register out of range")
                            .borrow()
                            .clone(),
                    )
                }
                match func {
                    Value::Function(kind) => match kind {
                        FunctionKind::Function(function) => self.call(&function, dst),
                        FunctionKind::UserFunction(func) => {
                            let value = func(args).map_err(|err| {
                                Located::new(RunTimeError::Custom(err.to_string()), pos)
                            })?;
                            if let Some(dst) = dst {
                                *frame.location(&dst).expect("dst not found").borrow_mut() = value;
                            }
                        }
                    },
                    _ => todo!(),
                };
            }
            ByteCode::Return { src } => return Ok(self.return_call(src)),
            ByteCode::Move { dst, src } => {
                let dst = frame.location(&dst).expect("location not found");
                let value = frame.source(&src).expect("source not found");
                *dst.borrow_mut() = value;
            }
            ByteCode::Field { dst, head, field } => {
                let dst = frame.location(&dst).expect("location not found");
                let head = frame.source(&head).expect("source not found");
                let field = frame.source(&field).expect("source not found");
                *dst.borrow_mut() = match head {
                    Value::Object(object) => match field {
                        Value::String(key) => object.borrow_mut().fields.get(&key).cloned(),
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::Object(Default::default()).typ(),
                                    field: field.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    Value::Vector(vector) => match field {
                        Value::Int(index) => {
                            let vector = vector.borrow_mut();
                            let length = vector.len();
                            let index = if index < 0 {
                                let index = length as i64 + index;
                                if index < 0 {
                                    length
                                } else {
                                    index as usize
                                }
                            } else {
                                index.unsigned_abs() as usize
                            };
                            vector.get(index).cloned()
                        }
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::Vector(Default::default()).typ(),
                                    field: field.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    Value::String(string) => match field {
                        Value::Int(index) => {
                            let length = string.len();
                            let index = if index < 0 {
                                let index = length as i64 + index;
                                if index < 0 {
                                    length
                                } else {
                                    index as usize
                                }
                            } else {
                                index.unsigned_abs() as usize
                            };
                            string
                                .get(index..=index)
                                .and_then(|s| s.chars().next())
                                .map(Value::Char)
                        }
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::String(Default::default()).typ(),
                                    field: field.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    head => return Err(Located::new(RunTimeError::CannotField(head.typ()), pos)),
                }
                .unwrap_or_default();
            }
            ByteCode::Vector { dst, start, amount } => {
                let dst = frame.location(&dst).expect("location not found");
                let mut values = vec![];
                for register in start..start + amount {
                    values.push(
                        frame
                            .register(register)
                            .expect("register not found")
                            .borrow()
                            .clone(),
                    );
                }
                *dst.borrow_mut() = Value::Vector(Rc::new(RefCell::new(values)));
            }
            ByteCode::Object { dst, start, amount } => {
                let dst = frame.location(&dst).expect("location not found");
                let mut object = Object::default();
                for register in (start..start + amount * 2).step_by(2) {
                    let Value::String(field) = frame
                        .register(register)
                        .expect("register not found")
                        .borrow()
                        .clone()
                    else {
                        panic!("expected object field to be a string")
                    };
                    let value = frame
                        .register(register + 1)
                        .expect("register not found")
                        .borrow()
                        .clone();
                    object.fields.insert(field, value);
                }
                *dst.borrow_mut() = Value::Object(Rc::new(RefCell::new(object)));
            }
            ByteCode::Binary {
                op,
                dst,
                left,
                right,
            } => {
                let dst = frame.location(&dst).expect("location not found");
                let left = frame.source(&left).expect("source not found");
                let right = frame.source(&right).expect("source not found");
                *dst.borrow_mut() = match op {
                    BinaryOperation::Add => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left + right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f64 + right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left + right as f64)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::Sub => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left - right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f64 - right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left - right as f64)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::Mul => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left * right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f64 * right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left * right as f64)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::Div => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            Value::Float(left as f64 / right as f64)
                        }
                        (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f64 / right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left / right as f64)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::Pow => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => {
                            Value::Float((left as f64).powf(right as f64))
                        }
                        (Value::Float(left), Value::Float(right)) => Value::Float(left.powf(right)),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float((left as f64).powf(right))
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left.powf(right as f64))
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::Mod => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left % right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left % right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f64 % right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left % right as f64)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::EQ => Value::Bool(left == right),
                    BinaryOperation::NE => Value::Bool(left != right),
                    BinaryOperation::LT => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Bool(left < right),
                        (Value::Float(left), Value::Float(right)) => Value::Bool(left < right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Bool((left as f64) < right)
                        }
                        (Value::Float(left), Value::Int(right)) => Value::Bool(left < right as f64),
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::GT => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Bool(left > right),
                        (Value::Float(left), Value::Float(right)) => Value::Bool(left > right),
                        (Value::Int(left), Value::Float(right)) => Value::Bool(left as f64 > right),
                        (Value::Float(left), Value::Int(right)) => Value::Bool(left > right as f64),
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::LE => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Bool(left <= right),
                        (Value::Float(left), Value::Float(right)) => Value::Bool(left <= right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Bool(left as f64 <= right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Bool(left <= right as f64)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::GE => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Bool(left >= right),
                        (Value::Float(left), Value::Float(right)) => Value::Bool(left >= right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Bool(left as f64 >= right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Bool(left >= right as f64)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.typ(),
                                    right: right.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    BinaryOperation::And => Value::Bool(left.into() && right.into()),
                    BinaryOperation::Or => Value::Bool(left.into() || right.into()),
                };
            }
            ByteCode::Unary { op, dst, src } => {
                let dst = frame.location(&dst).expect("location not found");
                let src = frame.source(&src).expect("source not found");
                *dst.borrow_mut() = match op {
                    UnaryOperation::Neg => match src {
                        Value::Int(v) => Value::Int(-v),
                        Value::Float(v) => Value::Float(-v),
                        src => {
                            return Err(Located::new(
                                RunTimeError::InvalidUnary {
                                    op,
                                    right: src.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    UnaryOperation::Not => Value::Bool(!bool::from(src)),
                    UnaryOperation::Len => match src {
                        Value::String(v) => Value::Int(v.len() as i64),
                        Value::Vector(v) => Value::Int(v.borrow().len() as i64),
                        src => {
                            return Err(Located::new(
                                RunTimeError::InvalidUnary {
                                    op,
                                    right: src.typ(),
                                },
                                pos,
                            ))
                        }
                    },
                };
            }
        }
        Ok(None)
    }
    pub fn run(&mut self) -> Result<Option<Value>, Located<RunTimeError>> {
        loop {
            if let Some(value) = self.step()? {
                return Ok(Some(value));
            }
            if self.call_frames.is_empty() {
                break;
            }
        }
        Ok(None)
    }
}
impl CallFrame {
    pub fn register(&self, register: usize) -> Option<Rc<RefCell<Value>>> {
        self.stack.get(register).cloned()
    }
    pub fn source(&self, source: &Source) -> Option<Value> {
        match source {
            Source::Register(register) => {
                self.register(*register).map(|value| value.borrow().clone())
            }
            Source::Upvalue(addr) => self
                .function
                .upvalues
                .get(*addr)
                .map(|value| value.borrow().clone()),
            Source::Global(addr) => {
                if let Value::String(ident) = self
                    .function
                    .closure
                    .borrow()
                    .consts
                    .get(*addr)
                    .expect("constant not found")
                {
                    Some(self.globals.borrow().get(ident).map(|value| value.borrow().clone()).unwrap_or_default())
                } else {
                    panic!("expected a string for the global")
                }
            }
            Source::Constant(addr) => self.function.closure.borrow().consts.get(*addr).cloned(),
            Source::Null => Some(Value::default()),
            Source::Bool(v) => Some(Value::Bool(*v)),
            Source::Char(v) => Some(Value::Char(*v)),
        }
    }
    pub fn location(&mut self, location: &Location) -> Option<Rc<RefCell<Value>>> {
        match location {
            Location::Register(register) => self.register(*register),
            Location::Upvalue(addr) => self.function.upvalues.get(*addr).map(Rc::clone),
            Location::Global(addr) => {
                if let Value::String(ident) = self
                    .function
                    .closure
                    .borrow()
                    .consts
                    .get(*addr)
                    .expect("constant not found")
                {
                    Some(self.globals.borrow().get(ident).cloned().unwrap_or_default())
                } else {
                    panic!("expected a string for the global")
                }
            }
        }
    }
}
impl Display for RunTimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunTimeError::CannotCall(typ) => write!(f, "can not call {typ}"),
            RunTimeError::CannotFieldInto { head, field } => {
                write!(f, "can not field into {head} with {field}")
            }
            RunTimeError::CannotField(typ) => write!(f, "can not field into {typ}"),
            RunTimeError::InvalidBinary { op, left, right } => write!(
                f,
                "attempt to perform binary operation {:?} on {left} with {right}",
                format!("{:?}", op).to_lowercase()
            ),
            RunTimeError::InvalidUnary { op, right } => write!(
                f,
                "attempt to perform unary operation {:?} on {right}",
                format!("{:?}", op).to_lowercase()
            ),
            RunTimeError::Custom(msg) => write!(f, "{msg}"),
        }
    }
}
impl Error for RunTimeError {}
