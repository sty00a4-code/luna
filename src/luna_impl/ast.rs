use crate::luna_impl::position::Located;

use super::tokens::Token;
use crate::lang::code::BinaryOperation;

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk(pub Vec<Located<Statement>>);
#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Located<Statement>>);
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Block),
    LetBinding {
        params: Vec<Located<Parameter>>,
        exprs: Vec<Located<Expression>>,
    },
    LetElse {
        param: Located<Parameter>,
        expr: Located<Expression>,
        else_case: Located<Block>,
    },
    Assign {
        paths: Vec<Located<Path>>,
        exprs: Vec<Located<Expression>>,
    },
    AssignOperation {
        op: AssignOperator,
        path: Located<Path>,
        expr: Located<Expression>,
    },
    Call {
        path: Located<Path>,
        args: Vec<Located<Expression>>,
    },
    SelfCall {
        head: Located<Path>,
        field: Located<String>,
        args: Vec<Located<Expression>>,
    },
    Fn {
        path: Located<Path>,
        params: Vec<Located<Parameter>>,
        var_args: Option<Located<String>>,
        body: Located<Block>,
    },
    LetFn {
        ident: Located<String>,
        params: Vec<Located<Parameter>>,
        var_args: Option<Located<String>>,
        body: Located<Block>,
    },
    If {
        cond: Located<Expression>,
        case: Located<Block>,
        else_case: Option<Located<Block>>,
    },
    IfLet {
        param: Located<Parameter>,
        expr: Located<Expression>,
        case: Located<Block>,
        else_case: Option<Located<Block>>,
    },
    Match {
        expr: Located<Expression>,
        cases: Vec<(Located<Pattern>, Located<Block>)>,
    },
    While {
        cond: Located<Expression>,
        body: Located<Block>,
    },
    WhileLet {
        param: Located<Parameter>,
        expr: Located<Expression>,
        body: Located<Block>,
    },
    For {
        ident: Located<String>,
        iter: Located<Expression>,
        body: Located<Block>,
    },
    Return(Option<Located<Expression>>),
    Break,
    Continue,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssignOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Exponent,
    Percent,
    // Ampersand,
    // Pipe,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(String),
    Atom(Atom),
    Guard {
        pattern: Box<Located<Self>>,
        cond: Located<Expression>,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Atom(Atom),
    Binary {
        op: BinaryOperator,
        left: Box<Located<Self>>,
        right: Box<Located<Self>>,
    },
    Unary {
        op: UnaryOperator,
        right: Box<Located<Self>>,
    },
    Call {
        head: Box<Located<Self>>,
        args: Vec<Located<Expression>>,
    },
    SelfCall {
        head: Box<Located<Self>>,
        field: Located<String>,
        args: Vec<Located<Expression>>,
    },
    Field {
        head: Box<Located<Self>>,
        field: Located<String>,
    },
    Index {
        head: Box<Located<Self>>,
        index: Box<Located<Expression>>,
    },
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Exponent,
    Percent,
    EqualEqual,
    ExclamationEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Ampersand,
    Pipe,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Minus,
    Exclamation,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Path(Path),
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Expression(Box<Located<Expression>>),
    Vector(Vec<Located<Expression>>),
    Object(Vec<(Located<String>, Located<Expression>)>),
    If {
        cond: Box<Located<Expression>>,
        case: Box<Located<Expression>>,
        else_case: Box<Located<Expression>>,
    },
    Fn {
        params: Vec<Located<Parameter>>,
        var_args: Option<Located<String>>,
        body: Located<Block>,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub enum Path {
    Ident(String),
    Field {
        head: Box<Located<Self>>,
        field: Located<String>,
    },
    Index {
        head: Box<Located<Self>>,
        index: Box<Located<Expression>>,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub enum Parameter {
    Ident(String),
    Object(Vec<Located<String>>),
    Vector(Vec<Located<String>>),
}

impl TryFrom<&Token> for AssignOperator {
    type Error = ();
    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::PlusEqual => Ok(Self::Plus),
            Token::MinusEqual => Ok(Self::Minus),
            Token::StarEqual => Ok(Self::Star),
            Token::SlashEqual => Ok(Self::Slash),
            Token::PercentEqual => Ok(Self::Percent),
            Token::ExponentEqual => Ok(Self::Exponent),
            _ => Err(()),
        }
    }
}
impl From<AssignOperator> for BinaryOperation {
    fn from(value: AssignOperator) -> Self {
        match value {
            AssignOperator::Plus => Self::Add,
            AssignOperator::Minus => Self::Sub,
            AssignOperator::Star => Self::Mul,
            AssignOperator::Slash => Self::Div,
            AssignOperator::Exponent => Self::Pow,
            AssignOperator::Percent => Self::Mod,
        }
    }
}
impl BinaryOperator {
    pub const LAYERS: &'static [&'static [Self]] = &[
        &[Self::Ampersand, Self::Pipe],
        &[
            Self::EqualEqual,
            Self::ExclamationEqual,
            Self::Less,
            Self::Greater,
            Self::LessEqual,
            Self::GreaterEqual,
        ],
        &[Self::Plus, Self::Minus],
        &[Self::Star, Self::Slash, Self::Percent],
        &[Self::Exponent],
    ];
    pub fn layer(layer: usize) -> Option<&'static [Self]> {
        Self::LAYERS.get(layer).cloned()
    }
}
impl TryFrom<&Token> for BinaryOperator {
    type Error = ();
    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Ampersand => Ok(Self::Ampersand),
            Token::Pipe => Ok(Self::Pipe),
            Token::Plus => Ok(Self::Plus),
            Token::Minus => Ok(Self::Minus),
            Token::Star => Ok(Self::Star),
            Token::Slash => Ok(Self::Slash),
            Token::Percent => Ok(Self::Percent),
            Token::Exponent => Ok(Self::Exponent),
            Token::EqualEqual => Ok(Self::EqualEqual),
            Token::ExclamationEqual => Ok(Self::ExclamationEqual),
            Token::Less => Ok(Self::Less),
            Token::Greater => Ok(Self::Greater),
            Token::LessEqual => Ok(Self::LessEqual),
            Token::GreaterEqual => Ok(Self::GreaterEqual),
            _ => Err(()),
        }
    }
}
impl UnaryOperator {
    pub const LAYERS: &'static [&'static [Self]] = &[&[Self::Exclamation], &[Self::Minus]];
    pub fn layer(layer: usize) -> Option<&'static [Self]> {
        Self::LAYERS.get(layer).cloned()
    }
}
impl TryFrom<&Token> for UnaryOperator {
    type Error = ();
    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Exclamation => Ok(Self::Exclamation),
            Token::Minus => Ok(Self::Minus),
            _ => Err(()),
        }
    }
}
