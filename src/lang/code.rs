use std::{cell::RefCell, fmt::Display, rc::Rc};
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
        cond: Source,
        addr: usize,
    },
    Iter {
        dst: Location,
        src: Source,
    },
    Next {
        dst: Location,
        src: Source,
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
    SetField {
        head: Source,
        field: Source,
        src: Source,
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
    Function {
        dst: Location,
        addr: usize,
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

#[derive(Debug, Clone, Default)]
pub struct Closure {
    pub code: Vec<Located<ByteCode>>,
    pub registers: usize,
    pub closures: Vec<Rc<RefCell<Self>>>,
    pub upvalues: Vec<Upvalue>,
    pub consts: Vec<Value>,
}
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Upvalue {
    pub register: usize,
    pub in_stack: bool,
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
            Self::Jump { addr } => write!(f, "jump *{addr:?}"),
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
            Self::JumpNull { cond, addr } => {
                write!(f, "jumpnull {cond} *{addr:?}")
            }
            Self::Iter { dst, src } => write!(f, "iter {dst} = {src}"),
            Self::Next { dst, src } => write!(f, "next {dst} = {src}"),
            Self::Call {
                dst,
                func,
                offset,
                amount,
            } => {
                if let Some(dst) = dst {
                    write!(f, "call {func} @{offset}..+{amount} -> {dst}")
                } else {
                    write!(f, "call {func} @{offset}..+{amount}")
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
            Self::SetField { head, field, src } => write!(f, "setfield {head} . {field} = {src}"),
            Self::Vector { dst, start, amount } => write!(f, "vector {dst} = @{start}..+{amount}"),
            Self::Object { dst, start, amount } => write!(f, "object {dst} = @{start}..+{amount}"),
            Self::Function { dst, addr } => write!(f, "func {dst} = #f{addr:?}"),
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
impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "closure {:08x?}", self as *const Closure)?;
        writeln!(f, "registers: {}", self.registers)?;
        writeln!(f, "code, #{} instructions:", self.code.len())?;
        for (addr, bytecode) in self.code.iter().enumerate() {
            writeln!(f, "\t[{addr:04}] {bytecode}")?;
        }
        writeln!(f, "upvalues:")?;
        for (addr, upvalue) in self.upvalues.iter().enumerate() {
            writeln!(
                f,
                "\t[{addr}] register: {}, in_stack: {}",
                upvalue.register, upvalue.in_stack
            )?;
        }
        writeln!(f, "constants:")?;
        for (addr, value) in self.consts.iter().enumerate() {
            writeln!(f, "\t[{addr}] {}: {:?}", value.typ(), value)?;
        }
        writeln!(f, "closures:")?;
        for (addr, closure) in self.closures.iter().enumerate() {
            writeln!(f, "\t[{addr}] {:08x?}", closure.as_ptr())?;
        }
        writeln!(f, "")?;
        for closure in self.closures.iter() {
            write!(f, "{}", closure.borrow())?;
        }
        Ok(())
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
