use crate::position::Located;

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk(pub Vec<Located<Statement>>);
#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Located<Statement>>);
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Block),
    LetBinding {
        idents: Vec<Located<String>>,
        exprs: Vec<Located<Expression>>,
    },
    Assign {
        paths: Vec<Located<Path>>,
        exprs: Vec<Located<Expression>>,
    },
    Call {
        path: Located<Path>,
        args: Vec<Located<Expression>>,
    },
    Fn {
        path: Located<Path>,
        params: Vec<Located<String>>,
        var_args: Located<String>,
        body: Located<Block>,
    },
    If {
        cond: Located<Expression>,
        case: Located<Block>,
        else_case: Option<Located<Block>>,
    },
    While {
        cond: Located<Expression>,
        body: Located<Block>,
    },
    For {
        idents: Vec<Located<String>>,
        iter: Located<Expression>,
        body: Located<Block>,
    },
    Return(Located<Expression>),
    Break,
    Continue,
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
        path: Located<Atom>,
        args: Vec<Located<Expression>>,
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
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
    And,
    Or,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Minus,
    Hashtag,
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
    If {
        cond: Box<Located<Expression>>,
        case: Box<Located<Expression>>,
        else_vase: Box<Located<Expression>>,
    },
    Fn {
        params: Vec<Located<String>>,
        var_args: Located<String>,
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
