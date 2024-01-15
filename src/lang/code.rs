use std::rc::Rc;

use super::value::Value;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ByteCode {
    #[default]
    None,
    
    Jump {
        addr: usize
    },
    JumpIf {
        negative: bool,
        cond: Source,
        addr: usize
    },

    Call {
        dst: Option<Location>,
        func: Source,
        offset: usize,
        amount: usize,
    },
    Return {
        src: Option<Source>
    },

    Move {
        dst: Location,
        src: Source
    },
    Field {
        dst: Location,
        head: Source,
        field: Source,
    },
    
    Binary {
        op: BinaryOperation,
        dst: Location,
        left: Source,
        right: Source
    },
    Unary {
        op: UnaryOperation,
        dst: Location,
        src: Source
    },
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Source {
    Register(usize),
    Upvalue(usize),
    Constant(usize),
    Null,
    Bool(bool),
    Char(char),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Register(usize),
    Upvalue(usize),
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

#[derive(Clone, Default)]
pub struct Closure {
    code: Vec<ByteCode>,
    parent: Option<Rc<Self>>,
    children: Option<Vec<Rc<Self>>>,
    upvalues: Vec<Upvalue>,
    consts: Vec<Value>,
}
#[derive(Clone, PartialEq, Default)]
pub struct Upvalue {
    register: usize,
    in_stack: bool
}