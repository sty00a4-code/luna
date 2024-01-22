use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),

    Equal,
    Comma,
    Dot,
    Exclamation,
    ParanLeft,
    ParanRight,
    BracketLeft,
    BracketRight,
    BraceLeft,
    BraceRight,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Exponent,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    ExponentEqual,
    EqualEqual,
    ExclamationEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    Ampersand,
    Pipe,

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

impl Token {
    pub fn name(&self) -> &'static str {
        self.into()
    }
    pub fn ident(s: String) -> Self {
        match s.as_str() {
            "null" => Self::Null,
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
            "let" => Self::Let,
            "fn" => Self::Fn,
            "if" => Self::If,
            "else" => Self::Else,
            "while" => Self::While,
            "for" => Self::For,
            "in" => Self::In,
            "return" => Self::Return,
            "break" => Self::Break,
            "continue" => Self::Continue,
            _ => Self::Ident(s)
        }
    }
}
impl From<&Token> for &'static str {
    fn from(val: &Token) -> Self {
        match val {
            Token::Ident(_) => "<ident>",
            Token::Null => "<null>",
            Token::Int(_) => "<int>",
            Token::Float(_) => "<float>",
            Token::Bool(_) => "<bool>",
            Token::Char(_) => "<char>",
            Token::String(_) => "<string>",
            Token::Equal => "=",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Exclamation => "!",
            Token::ParanLeft => "(",
            Token::ParanRight => ")",
            Token::BracketLeft => "[",
            Token::BracketRight => "]",
            Token::BraceLeft => "{",
            Token::BraceRight => "}",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Percent => "%",
            Token::Exponent => "^",
            Token::PlusEqual => "+=",
            Token::MinusEqual => "-=",
            Token::StarEqual => "*=",
            Token::SlashEqual => "/=",
            Token::PercentEqual => "%=",
            Token::ExponentEqual => "^=",
            Token::EqualEqual => "==",
            Token::ExclamationEqual => "!=",
            Token::Less => "<",
            Token::Greater => ">",
            Token::LessEqual => "<=",
            Token::GreaterEqual => ">=",
            Token::Ampersand => "&",
            Token::Pipe => "|",
            Token::Let => "let",
            Token::Fn => "fn",
            Token::If => "if",
            Token::Else => "else",
            Token::While => "while",
            Token::For => "for",
            Token::In => "in",
            Token::Return => "return",
            Token::Break => "break",
            Token::Continue => "continue",
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Null => write!(f, "null"),
            Token::Int(v) => write!(f, "{v:?}"),
            Token::Float(v) => write!(f, "{v:?}"),
            Token::Bool(v) => write!(f, "{v:?}"),
            Token::Char(v) => write!(f, "{v:?}"),
            Token::String(v) => write!(f, "{v:?}"),
            _ => write!(f, "{:?}", std::convert::Into::<&'static str>::into(self))
        }
    }
}