#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),

    And,
    Or,
    Not,

    Let,
    Fn,
    If,
    Else,
    While,
    For,
    In,
    Return,
    Break,
    Continue,
}
