use std::fmt::Display;
// use std::{
//     cell::RefCell,
//     rc::Rc
// };

use crate::luna_impl::position::Located;

use super::{
    ast::{BinaryOperator, UnaryOperator},
    value::Value,
};
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ByteCode {
    #[default]
    None,

    Jump {
        addr: usize,
    },
    JumpIf {
        negative: bool,
        cond: Source,
        addr: usize,
    },
    JumpNull {
        negative: bool,
        cond: Source,
        addr: usize,
    },

    Call {
        dst: Option<Location>,
        func: Source,
        offset: usize,
        amount: usize,
    },
    Return {
        src: Option<Source>,
    },

    Move {
        dst: Location,
        src: Source,
    },
    Field {
        dst: Location,
        head: Source,
        field: Source,
    },

    Vector {
        dst: Location,
        start: usize,
        amount: usize,
    },
    Object {
        dst: Location,
        start: usize,
        amount: usize,
    },

    Binary {
        op: BinaryOperation,
        dst: Location,
        left: Source,
        right: Source,
    },
    Unary {
        op: UnaryOperation,
        dst: Location,
        src: Source,
    },
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Source {
    Register(usize),
    Upvalue(usize),
    Global(usize),
    Constant(usize),
    #[default]
    Null,
    Bool(bool),
    Char(char),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Register(usize),
    Upvalue(usize),
    Global(usize),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
    And,
    Or,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperation {
    Neg,
    Not,
    Len,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Closure {
    pub code: Vec<Located<ByteCode>>,
    pub registers: usize,
    // pub parent: Option<Rc<RefCell<Self>>>,
    // pub children: Vec<Rc<RefCell<Self>>>,
    pub upvalues: Vec<Upvalue>,
    pub consts: Vec<Value>,
}
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Upvalue {
    pub register: usize,
    pub in_stack: bool,
}

impl Closure {
    pub fn new_const(&mut self, value: Value) -> usize {
        let addr = self.consts.len();
        self.consts.push(value);
        addr
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(reg) => write!(f, "@{reg}"),
            Self::Upvalue(addr) => write!(f, "@u{addr}"),
            Self::Global(addr) => write!(f, "@g{addr}"),
            Self::Constant(addr) => write!(f, "#{addr}"),
            Self::Null => write!(f, "null"),
            Self::Bool(v) => write!(f, "{v:?}"),
            Self::Char(v) => write!(f, "{v:?}"),
        }
    }
}
impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(reg) => write!(f, "!{reg}"),
            Self::Upvalue(addr) => write!(f, "!u{addr}"),
            Self::Global(addr) => write!(f, "!g{addr}"),
        }
    }
}
impl Display for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Jump { addr } => write!(f, "jump {addr:04x?}"),
            Self::JumpIf {
                negative,
                cond,
                addr,
            } => {
                if *negative {
                    write!(f, "jumpifnot {cond} *{addr:?}")
                } else {
                    write!(f, "jumpif {cond} *{addr:?}")
                }
            }
            Self::JumpNull {
                negative,
                cond,
                addr,
            } => {
                if *negative {
                    write!(f, "jumpnullnot {cond} *{addr:?}")
                } else {
                    write!(f, "jumpnull {cond} *{addr:?}")
                }
            }
            Self::Call {
                dst,
                func,
                offset,
                amount,
            } => {
                if let Some(dst) = dst {
                    write!(f, "call {func} @{offset}..+@{amount} -> {dst}")
                } else {
                    write!(f, "call {func} @{offset}..+@{amount}")
                }
            }
            Self::Return { src } => {
                if let Some(src) = src {
                    write!(f, "return {src}")
                } else {
                    write!(f, "return")
                }
            }
            Self::Move { dst, src } => write!(f, "move {dst} = {src}"),
            Self::Field { dst, head, field } => write!(f, "field {dst} = {head} . {field}"),
            Self::Vector { dst, start, amount } => write!(f, "vector {dst} = @{start}..+@{amount}"),
            Self::Object { dst, start, amount } => write!(f, "object {dst} = @{start}..+@{amount}"),
            Self::Binary {
                op,
                dst,
                left,
                right,
            } => write!(
                f,
                "binary {dst} = {left} {} {right}",
                format!("{op:?}").to_lowercase()
            ),
            Self::Unary { op, dst, src } => write!(
                f,
                "unary {dst} = {} {src}",
                format!("{op:?}").to_lowercase()
            ),
        }
    }
}

impl From<Location> for Source {
    fn from(value: Location) -> Self {
        match value {
            Location::Register(register) => Self::Register(register),
            Location::Upvalue(addr) => Self::Upvalue(addr),
            Location::Global(addr) => Self::Global(addr),
        }
    }
}

impl From<BinaryOperator> for BinaryOperation {
    fn from(value: BinaryOperator) -> Self {
        match value {
            BinaryOperator::Plus => Self::Add,
            BinaryOperator::Minus => Self::Sub,
            BinaryOperator::Star => Self::Mul,
            BinaryOperator::Slash => Self::Div,
            BinaryOperator::Exponent => Self::Pow,
            BinaryOperator::Percent => Self::Mod,
            BinaryOperator::EqualEqual => Self::EQ,
            BinaryOperator::ExclamationEqual => Self::NE,
            BinaryOperator::Less => Self::LT,
            BinaryOperator::Greater => Self::GT,
            BinaryOperator::LessEqual => Self::LE,
            BinaryOperator::GreaterEqual => Self::GE,
            BinaryOperator::Ampersand => Self::And,
            BinaryOperator::Pipe => Self::Or,
        }
    }
}
impl From<UnaryOperator> for UnaryOperation {
    fn from(value: UnaryOperator) -> Self {
        match value {
            UnaryOperator::Minus => Self::Neg,
            UnaryOperator::Exclamation => Self::Not,
        }
    }
}
