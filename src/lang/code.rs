use super::value::Value;
use crate::luna_impl::{
    ast::{BinaryOperator, UnaryOperator},
    position::Located,
};
use std::{cell::RefCell, fmt::Display, rc::Rc};

pub type Register = u16;
pub type Address = u32;
pub type VectorSize = u16;
pub type ObjectSize = u16;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ByteCode {
    #[default]
    None, // 0

    Jump {
        // 1
        addr: Address,
    },
    JumpIf {
        // 2 - 15 : u32 u32
        negative: bool,
        cond: Source,
        addr: Address,
    },
    JumpNull {
        // 16 - 22 : u32 u32
        negative: bool,
        cond: Source,
        addr: Address,
    },

    CallZero {
        // 44 - 71 : u32 u32 u32 u32
        dst: Option<Location>,
        func: Source,
    },
    CallSingle {
        // 44 - 71 : u32 u32 u32 u32
        dst: Option<Location>,
        func: Source,
        arg: Source,
    },
    Call {
        // 44 - 71 : u32 u32 u32 u32
        dst: Option<Location>,
        func: Source,
        offset: Register,
        amount: u8,
    },
    Return {
        // 72 - 79 : u32
        src: Option<Source>,
    },

    Move {
        // 80 - 100 : u32 u32
        dst: Location,
        src: Source,
    },
    Field {
        // 101 - 247 : u32 u32 u32
        dst: Location,
        head: Source,
        field: Source,
    },
    SetField {
        // 248 - 590 : u32 u32 u32
        head: Source,
        field: Source,
        src: Source,
    },

    Vector {
        // 591 - 593 : u32 u32 u32
        dst: Location,
        start: Register,
        amount: VectorSize,
    },
    Object {
        // 594 - 596 : u32 u32 u32
        dst: Location,
        start: Register,
        amount: ObjectSize,
    },
    Function {
        // 597 - 599 : u32 u32
        dst: Location,
        addr: Address,
    },

    Binary {
        // 600 - 746 : u8 u32 u32 u32
        op: BinaryOperation,
        dst: Location,
        left: Source,
        right: Source,
    },
    Unary {
        // 747 - 767 : u8 u32 u32
        op: UnaryOperation,
        dst: Location,
        src: Source,
    },
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Source {
    Register(Register),
    Upvalue(Address),
    Global(Address),
    Constant(Address),
    #[default]
    Null,
    Bool(bool),
    Char(char),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Register(Register),
    Upvalue(Address),
    Global(Address),
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
}

#[derive(Debug, Clone, Default)]
pub struct Closure {
    pub code: Vec<Located<ByteCode>>,
    pub registers: Register,
    pub closures: Vec<Rc<RefCell<Self>>>,
    pub upvalues: Vec<Upvalue>,
    pub consts: Vec<Value>,
    pub path: Option<String>,
}
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Upvalue {
    pub register: Register,
    pub depth: u8,
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
            Self::JumpNull {
                negative,
                cond,
                addr,
            } => {
                if *negative {
                    write!(f, "jumpnotnull {cond} *{addr:?}")
                } else {
                    write!(f, "jumpnull {cond} *{addr:?}")
                }
            }
            Self::CallZero { dst, func } => {
                if let Some(dst) = dst {
                    write!(f, "call {func} -> {dst}")
                } else {
                    write!(f, "call {func}")
                }
            }
            Self::CallSingle { dst, func, arg } => {
                if let Some(dst) = dst {
                    write!(f, "call {func} {arg} -> {dst}")
                } else {
                    write!(f, "call {func} {arg}")
                }
            }
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
                "\t[{addr}] register: {}, depth: {}",
                upvalue.register, upvalue.depth
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
        writeln!(f)?;
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
impl Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Mul => write!(f, "mul"),
            Self::Div => write!(f, "div"),
            Self::Pow => write!(f, "pow"),
            Self::Mod => write!(f, "mod"),
            Self::EQ => write!(f, "eq"),
            Self::NE => write!(f, "ne"),
            Self::LT => write!(f, "lt"),
            Self::GT => write!(f, "gt"),
            Self::LE => write!(f, "le"),
            Self::GE => write!(f, "ge"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
        }
    }
}
impl Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperation::Neg => write!(f, "neg"),
            UnaryOperation::Not => write!(f, "not"),
        }
    }
}
