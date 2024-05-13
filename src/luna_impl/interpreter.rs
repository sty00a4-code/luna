use super::{
    position::{Located, Position},
    std::{
        globals, BOOL_MODULE, CHAR_MODULE, FLOAT_MODULE, INT_MODULE, STRING_MODULE, VECTOR_MODULE,
    },
};
use crate::lang::{
    code::{Address, BinaryOperation, ByteCode, Location, Register, Source, UnaryOperation},
    value::{Function, FunctionKind, Object, Value, META_CALL, META_GET, META_SET},
};
use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, rc::Rc};

pub const CALL_STACK_CAP: usize = 0xffff;

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub call_frames: Vec<CallFrame>,
    pub globals: Rc<RefCell<HashMap<String, Rc<RefCell<Value>>>>>,
    pub global_path: Option<String>,
}
#[derive(Debug, Clone)]
pub struct CallFrame {
    pub function: Rc<Function>,
    pub stack: Vec<Rc<RefCell<Value>>>,
    pub idx: Address,
    pub dst: Option<Location>,
    pub globals: Rc<RefCell<HashMap<String, Rc<RefCell<Value>>>>>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum RunTimeError {
    CannotCall(String),
    CannotIter(String),
    CannotFieldInto {
        head: String,
        field: String,
    },
    CannotField(String),
    CannotSetFieldWith {
        head: String,
        field: String,
    },
    CannotSetField(String),
    InvalidSetIndex(usize),
    InvalidBinary {
        op: BinaryOperation,
        left: String,
        right: String,
    },
    InvalidUnary {
        op: UnaryOperation,
        right: String,
    },
    Custom(String),
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            call_frames: Vec::with_capacity(CALL_STACK_CAP),
            globals: Rc::new(RefCell::new(globals())),
            global_path: None,
        }
    }
}
impl Interpreter {
    #[inline(always)]
    pub fn with_global_path(mut self, path: Option<String>) -> Self {
        self.global_path = path;
        self
    }
}
impl Interpreter {
    #[inline(always)]
    pub fn path(&self) -> Option<String> {
        self.call_frames
            .last()?
            .function
            .closure
            .borrow()
            .path
            .clone()
    }
    #[inline(always)]
    pub fn call(&mut self, function: &Rc<Function>, args: Vec<Value>, dst: Option<Location>) {
        let mut stack = Vec::with_capacity(function.closure.borrow().registers as usize + 1);
        let args_len = args.len();
        stack.extend(args.into_iter().map(|v| Rc::new(RefCell::new(v))));
        stack.extend(
            (args_len..function.closure.borrow().registers as usize + 1)
                .map(|_| Rc::new(RefCell::new(Value::default()))),
        );
        self.call_frames.push(CallFrame {
            function: Rc::clone(function),
            stack,
            idx: 0,
            dst,
            globals: Rc::clone(&self.globals),
        });
    }
    #[inline(always)]
    pub fn return_call(&mut self, src: Option<Source>) -> Option<Value> {
        let top_frame = self.call_frames.pop().expect("no frame on stack");
        if let Some(prev_frame) = self.call_frames.last_mut() {
            if let Some(dst) = top_frame.dst {
                let value = top_frame
                    .source(&src.unwrap_or_default())
                    .expect("source not found");
                let dst = prev_frame.location(&dst).expect("location not found");
                *dst.borrow_mut() = value;
            }
        }
        if let Some(src) = src {
            return Some(top_frame.source(&src).expect("source not found"));
        }
        None
    }
    #[inline(always)]
    pub fn call_kind(
        &mut self,
        kind: FunctionKind,
        args: Vec<Value>,
        dst: Option<Location>,
        pos: Position,
    ) -> Result<(), Located<RunTimeError>> {
        match kind {
            FunctionKind::Function(function) => {
                self.call(&function, args, dst);
                Ok(())
            }
            FunctionKind::UserFunction(func) => {
                let dst = dst.map(|dst| {
                    self.call_frames
                        .last_mut()
                        .expect("no call frame")
                        .location(&dst)
                        .expect("dst not found")
                });
                let value = func(self, args)
                    .map_err(|err| Located::new(RunTimeError::Custom(err.to_string()), pos))?;
                if let Some(dst) = dst {
                    *dst.borrow_mut() = value;
                }
                Ok(())
            }
        }
    }
    pub fn step(&mut self) -> Result<Option<Value>, Located<RunTimeError>> {
        let idx = self.call_frames.last_mut().expect("no call frame").idx;
        let Located {
            value: bytecode,
            pos,
        } = self
            .call_frames
            .last_mut()
            .expect("no call frame")
            .function
            .closure
            .borrow()
            .code
            .get(idx as usize)
            .cloned()
            .expect("idx out of range");
        self.call_frames.last_mut().expect("no call frame").idx += 1;
        match bytecode {
            ByteCode::None => {}
            ByteCode::Jump { addr } => {
                self.call_frames.last_mut().expect("no call frame").idx = addr;
            }
            ByteCode::JumpIf {
                negative: false,
                cond,
                addr,
            } => {
                if self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&cond)
                    .expect("cond not found")
                    .into()
                {
                    self.call_frames.last_mut().expect("no call frame").idx = addr;
                }
            }
            ByteCode::JumpIf {
                negative: true,
                cond,
                addr,
            } => {
                if !bool::from(
                    self.call_frames
                        .last_mut()
                        .expect("no call frame")
                        .source(&cond)
                        .expect("cond not found"),
                ) {
                    self.call_frames.last_mut().expect("no call frame").idx = addr;
                }
            }
            ByteCode::JumpNull {
                negative: false,
                cond,
                addr,
            } => {
                if self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&cond)
                    .expect("cond not found")
                    == Value::default()
                {
                    self.call_frames.last_mut().expect("no call frame").idx = addr;
                }
            }
            ByteCode::JumpNull {
                negative: true,
                cond,
                addr,
            } => {
                if self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&cond)
                    .expect("cond not found")
                    != Value::default()
                {
                    self.call_frames.last_mut().expect("no call frame").idx = addr;
                }
            }
            ByteCode::CallSingle { dst, func, arg } => {
                let func = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&func)
                    .expect("func not found");
                let mut args = vec![self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&arg)
                    .expect("source not found")];
                match func {
                    Value::Function(kind) => self.call_kind(kind, args, dst, pos)?,
                    Value::Object(object) => {
                        args.insert(0, Value::Object(Rc::clone(&object)));
                        let object = object.borrow();
                        if let Some(Value::Function(kind)) = object.get_meta(META_CALL) {
                            self.call_kind(kind, args, dst, pos)?;
                        }
                    }
                    value => {
                        return Err(Located::new(
                            RunTimeError::CannotCall(value.dynamic_typ()),
                            pos,
                        ))
                    }
                };
            }
            ByteCode::CallZero { dst, func } => {
                let func = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&func)
                    .expect("func not found");
                let mut args = Vec::with_capacity(1);
                match func {
                    Value::Function(kind) => self.call_kind(kind, args, dst, pos)?,
                    Value::Object(object) => {
                        args.insert(0, Value::Object(Rc::clone(&object)));
                        let object = object.borrow();
                        if let Some(Value::Function(kind)) = object.get_meta(META_CALL) {
                            self.call_kind(kind, args, dst, pos)?;
                        }
                    }
                    value => {
                        return Err(Located::new(
                            RunTimeError::CannotCall(value.dynamic_typ()),
                            pos,
                        ))
                    }
                };
            }
            ByteCode::Call {
                dst,
                func,
                offset,
                amount,
            } => {
                let func = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&func)
                    .expect("func not found");
                let mut args = Vec::with_capacity(amount as usize);
                for register in offset..offset + amount as Register {
                    args.push(
                        self.call_frames
                            .last_mut()
                            .expect("no call frame")
                            .register(register)
                            .expect("register out of range")
                            .borrow()
                            .clone(),
                    )
                }
                match func {
                    Value::Function(kind) => self.call_kind(kind, args, dst, pos)?,
                    Value::Object(object) => {
                        args.insert(0, Value::Object(Rc::clone(&object)));
                        let object = object.borrow();
                        if let Some(Value::Function(kind)) = object.get_meta(META_CALL) {
                            self.call_kind(kind, args, dst, pos)?;
                        }
                    }
                    value => {
                        return Err(Located::new(
                            RunTimeError::CannotCall(value.dynamic_typ()),
                            pos,
                        ))
                    }
                };
            }
            ByteCode::Return { src } => return Ok(self.return_call(src)),
            ByteCode::Move { dst, src } => {
                let dst = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .location(&dst)
                    .expect("location not found");
                let value = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&src)
                    .expect("source not found");
                *dst.borrow_mut() = value;
            }
            ByteCode::Field { dst, head, field } => {
                let dst_value = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .location(&dst)
                    .expect("location not found");
                let head = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&head)
                    .expect("source not found");
                let field = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&field)
                    .expect("source not found");
                *dst_value.borrow_mut() = match &head {
                    Value::Object(object) => {
                        if let Some(Value::Function(kind)) = object.borrow().get_meta(META_GET) {
                            self.call_kind(
                                kind,
                                vec![head.clone(), field],
                                Some(dst),
                                pos.clone(),
                            )?;
                            return Ok(None);
                        }
                        match field {
                            Value::String(key) => object.borrow_mut().fields.get(&key).cloned(),
                            field => {
                                return Err(Located::new(
                                    RunTimeError::CannotFieldInto {
                                        head: Value::Object(Default::default()).dynamic_typ(),
                                        field: field.dynamic_typ(),
                                    },
                                    pos,
                                ))
                            }
                        }
                    }
                    Value::UserObject(object) => match field {
                        Value::String(key) => object.borrow_mut().get(&key),
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::Object(Default::default()).dynamic_typ(),
                                    field: field.dynamic_typ(),
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
                        Value::String(key) => {
                            if let Value::Object(object) = self
                                .globals
                                .borrow()
                                .get(VECTOR_MODULE)
                                .cloned()
                                .unwrap_or_default()
                                .borrow()
                                .clone()
                            {
                                let object = object.borrow();
                                object.fields.get(&key).cloned()
                            } else {
                                Some(Value::default())
                            }
                        }
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::Vector(Default::default()).dynamic_typ(),
                                    field: field.dynamic_typ(),
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
                        Value::String(key) => {
                            if let Value::Object(object) = self
                                .globals
                                .borrow()
                                .get(STRING_MODULE)
                                .cloned()
                                .unwrap_or_default()
                                .borrow()
                                .clone()
                            {
                                let object = object.borrow();
                                object.fields.get(&key).cloned()
                            } else {
                                Some(Value::default())
                            }
                        }
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::String(Default::default()).dynamic_typ(),
                                    field: field.dynamic_typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    Value::Int(_) => match field {
                        Value::String(key) => {
                            if let Value::Object(object) = self
                                .globals
                                .borrow()
                                .get(INT_MODULE)
                                .cloned()
                                .unwrap_or_default()
                                .borrow()
                                .clone()
                            {
                                let object = object.borrow();
                                object.fields.get(&key).cloned()
                            } else {
                                Some(Value::default())
                            }
                        }
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::String(Default::default()).dynamic_typ(),
                                    field: field.dynamic_typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    Value::Float(_) => match field {
                        Value::String(key) => {
                            if let Value::Object(object) = self
                                .globals
                                .borrow()
                                .get(FLOAT_MODULE)
                                .cloned()
                                .unwrap_or_default()
                                .borrow()
                                .clone()
                            {
                                let object = object.borrow();
                                object.fields.get(&key).cloned()
                            } else {
                                Some(Value::default())
                            }
                        }
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::String(Default::default()).dynamic_typ(),
                                    field: field.dynamic_typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    Value::Bool(_) => match field {
                        Value::String(key) => {
                            if let Value::Object(object) = self
                                .globals
                                .borrow()
                                .get(BOOL_MODULE)
                                .cloned()
                                .unwrap_or_default()
                                .borrow()
                                .clone()
                            {
                                let object = object.borrow();
                                object.fields.get(&key).cloned()
                            } else {
                                Some(Value::default())
                            }
                        }
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::String(Default::default()).dynamic_typ(),
                                    field: field.dynamic_typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    Value::Char(_) => match field {
                        Value::String(key) => {
                            if let Value::Object(object) = self
                                .globals
                                .borrow()
                                .get(CHAR_MODULE)
                                .cloned()
                                .unwrap_or_default()
                                .borrow()
                                .clone()
                            {
                                let object = object.borrow();
                                object.fields.get(&key).cloned()
                            } else {
                                Some(Value::default())
                            }
                        }
                        field => {
                            return Err(Located::new(
                                RunTimeError::CannotFieldInto {
                                    head: Value::String(Default::default()).dynamic_typ(),
                                    field: field.dynamic_typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    head => {
                        return Err(Located::new(
                            RunTimeError::CannotField(head.dynamic_typ()),
                            pos,
                        ))
                    }
                }
                .unwrap_or_default();
            }
            ByteCode::SetField { head, field, src } => {
                let value = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&src)
                    .expect("source not found");
                let head = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&head)
                    .expect("source not found");
                let field = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&field)
                    .expect("source not found");
                match &head {
                    Value::Object(object) => {
                        if let Some(Value::Function(kind)) = object.borrow().get_meta(META_SET) {
                            self.call_kind(
                                kind,
                                vec![head.clone(), field, value],
                                None,
                                pos.clone(),
                            )?;
                            return Ok(None);
                        }
                        let mut object = object.borrow_mut();
                        let old_value = match field {
                            Value::String(key) => {
                                if let Some(value) = object.fields.get_mut(&key) {
                                    value
                                } else {
                                    object.fields.insert(key.clone(), Value::default());
                                    object.fields.get_mut(&key).unwrap()
                                }
                            }
                            field => {
                                return Err(Located::new(
                                    RunTimeError::CannotSetFieldWith {
                                        head: Value::Object(Default::default()).dynamic_typ(),
                                        field: field.dynamic_typ(),
                                    },
                                    pos,
                                ))
                            }
                        };
                        *old_value = value;
                    }
                    Value::Vector(vector) => {
                        let mut vector = vector.borrow_mut();
                        match field {
                            Value::Int(index) => {
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
                                if let Some(old_value) = vector.get_mut(index) {
                                    *old_value = value;
                                } else {
                                    return Err(Located::new(
                                        RunTimeError::InvalidSetIndex(index),
                                        pos,
                                    ));
                                }
                            }
                            field => {
                                return Err(Located::new(
                                    RunTimeError::CannotSetFieldWith {
                                        head: Value::Vector(Default::default()).dynamic_typ(),
                                        field: field.dynamic_typ(),
                                    },
                                    pos,
                                ))
                            }
                        }
                    }
                    head => {
                        return Err(Located::new(
                            RunTimeError::CannotSetField(head.dynamic_typ()),
                            pos,
                        ))
                    }
                }
            }
            ByteCode::Vector { dst, start, amount } => {
                let dst = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .location(&dst)
                    .expect("location not found");
                let mut values = vec![];
                for register in start..start + amount {
                    values.push(
                        self.call_frames
                            .last_mut()
                            .expect("no call frame")
                            .register(register)
                            .expect("register not found")
                            .borrow()
                            .clone(),
                    );
                }
                *dst.borrow_mut() = Value::Vector(Rc::new(RefCell::new(values)));
            }
            ByteCode::Object { dst, start, amount } => {
                let dst = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .location(&dst)
                    .expect("location not found");
                let mut object = Object::default();
                for register in (start..start + amount * 2).step_by(2) {
                    let Value::String(field) = self
                        .call_frames
                        .last_mut()
                        .expect("no call frame")
                        .register(register)
                        .expect("register not found")
                        .borrow()
                        .clone()
                    else {
                        panic!("expected object field to be a string")
                    };
                    let value = self
                        .call_frames
                        .last_mut()
                        .expect("no call frame")
                        .register(register + 1)
                        .expect("register not found")
                        .borrow()
                        .clone();
                    object.fields.insert(field, value);
                }
                *dst.borrow_mut() = Value::Object(Rc::new(RefCell::new(object)));
            }
            ByteCode::Function { dst, addr } => {
                let dst = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .location(&dst)
                    .expect("location not found");
                let closure = Rc::clone(
                    self.call_frames
                        .last_mut()
                        .expect("no call frame on stack")
                        .function
                        .closure
                        .borrow()
                        .closures
                        .get(addr as usize)
                        .expect("closure not found"),
                );
                let mut upvalues = Vec::with_capacity(closure.borrow().upvalues.len());
                for upvalue in closure.borrow().upvalues.iter() {
                    upvalues.push(if upvalue.depth == 0 {
                        self.call_frames
                            .last_mut()
                            .expect("no call frame")
                            .register(upvalue.register)
                            .expect("register not found")
                    } else if let Some(value) = self
                        .call_frames
                        .get(self.call_frames.len() - 1 - upvalue.depth as usize)
                        .map(|frame| {
                            frame
                                .register(upvalue.register)
                                .expect("register not found")
                        })
                    {
                        value
                    } else {
                        Rc::default()
                    });
                }
                *dst.borrow_mut() = Value::Function(FunctionKind::Function(Rc::new(Function {
                    upvalues,
                    closure,
                })));
            }
            ByteCode::Binary {
                op,
                dst,
                left,
                right,
            } => {
                let dst_value = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .location(&dst)
                    .expect("location not found");
                let left = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&left)
                    .expect("source not found");
                let right = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&right)
                    .expect("source not found");
                if let Value::Object(object) = &left {
                    if let Some(Value::Function(kind)) = {
                        let object = object.borrow();
                        object.get_meta(&format!("__{}", op))
                    } {
                        self.call_kind(kind, vec![left, right], Some(dst), pos)?;
                        return Ok(None);
                    }
                }
                *dst_value.borrow_mut() = match op {
                    BinaryOperation::Add => match (left, right) {
                        (Value::Int(left), Value::Int(right)) => Value::Int(left + right),
                        (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
                        (Value::Int(left), Value::Float(right)) => {
                            Value::Float(left as f64 + right)
                        }
                        (Value::Float(left), Value::Int(right)) => {
                            Value::Float(left + right as f64)
                        }
                        (Value::String(left), Value::String(right)) => Value::String(left + &right),
                        (Value::String(mut left), Value::Char(right)) => {
                            left.push(right);
                            Value::String(left)
                        }
                        (left, right) => {
                            return Err(Located::new(
                                RunTimeError::InvalidBinary {
                                    op,
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                                    left: left.dynamic_typ(),
                                    right: right.dynamic_typ(),
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
                let dst_value = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .location(&dst)
                    .expect("location not found");
                let src = self
                    .call_frames
                    .last_mut()
                    .expect("no call frame")
                    .source(&src)
                    .expect("source not found");
                if let Value::Object(object) = &src {
                    if let Some(Value::Function(kind)) = {
                        let object = object.borrow();
                        object.get_meta(&format!("__{}", op))
                    } {
                        self.call_kind(kind, vec![src], Some(dst), pos)?;
                        return Ok(None);
                    }
                }
                *dst_value.borrow_mut() = match op {
                    UnaryOperation::Neg => match src {
                        Value::Int(v) => Value::Int(-v),
                        Value::Float(v) => Value::Float(-v),
                        src => {
                            return Err(Located::new(
                                RunTimeError::InvalidUnary {
                                    op,
                                    right: src.dynamic_typ(),
                                },
                                pos,
                            ))
                        }
                    },
                    UnaryOperation::Not => Value::Bool(!bool::from(src)),
                };
            }
        }
        Ok(None)
    }
    pub fn run(&mut self) -> Result<Option<Value>, Located<RunTimeError>> {
        let level = self.call_frames.len();
        loop {
            let value = self.step()?;
            if self.call_frames.len() < level || self.call_frames.is_empty() {
                return Ok(value);
            }
        }
    }
}
impl CallFrame {
    #[inline(always)]
    pub fn path(&self) -> Option<String> {
        self.function.closure.borrow().path.clone()
    }
    #[inline(always)]
    pub fn register(&self, register: Register) -> Option<Rc<RefCell<Value>>> {
        self.stack.get(register as usize).cloned()
    }
    #[inline(always)]
    pub fn source(&self, source: &Source) -> Option<Value> {
        match source {
            Source::Register(register) => {
                self.register(*register).map(|value| value.borrow().clone())
            }
            Source::Upvalue(addr) => self
                .function
                .upvalues
                .get(*addr as usize)
                .map(|value| value.borrow().clone()),
            Source::Global(addr) => {
                if let Value::String(ident) = self
                    .function
                    .closure
                    .borrow()
                    .consts
                    .get(*addr as usize)
                    .expect("constant not found")
                {
                    Some(
                        self.globals
                            .borrow()
                            .get(ident)
                            .map(|value| value.borrow().clone())
                            .unwrap_or_default(),
                    )
                } else {
                    panic!("expected a string for the global")
                }
            }
            Source::Constant(addr) => self
                .function
                .closure
                .borrow()
                .consts
                .get(*addr as usize)
                .cloned(),
            Source::Null => Some(Value::default()),
            Source::Bool(v) => Some(Value::Bool(*v)),
            Source::Char(v) => Some(Value::Char(*v)),
        }
    }
    #[inline(always)]
    pub fn location(&mut self, location: &Location) -> Option<Rc<RefCell<Value>>> {
        match location {
            Location::Register(register) => self.register(*register),
            Location::Upvalue(addr) => self.function.upvalues.get(*addr as usize).map(Rc::clone),
            Location::Global(addr) => {
                if let Value::String(ident) = self
                    .function
                    .closure
                    .borrow()
                    .consts
                    .get(*addr as usize)
                    .expect("constant not found")
                {
                    let mut globals = self.globals.borrow_mut();
                    if let Some(value) = globals.get(ident).cloned() {
                        Some(value)
                    } else {
                        globals.insert(ident.clone(), Rc::new(RefCell::new(Value::default())));
                        globals.get(ident).cloned()
                    }
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
            RunTimeError::CannotIter(typ) => write!(f, "can not iterate over {typ}"),
            RunTimeError::CannotFieldInto { head, field } => {
                write!(f, "can not field into {head} with {field}")
            }
            RunTimeError::CannotField(typ) => write!(f, "can not field into {typ}"),
            RunTimeError::CannotSetFieldWith { head, field } => {
                write!(f, "can not set field of {head} with {field}")
            }
            RunTimeError::CannotSetField(head) => write!(f, "can not set field of {head}"),
            RunTimeError::InvalidSetIndex(index) => {
                write!(f, "invalid set index {index:?} (out of range)")
            }
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
