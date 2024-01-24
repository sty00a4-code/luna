use std::{
    error::Error,
    fmt::Display,
    iter::Peekable,
    num::{ParseFloatError, ParseIntError},
    str::Chars,
};

use crate::lang::tokens::Token;

use super::position::{Located, Position};

#[derive(Debug, Clone)]
pub struct Lexer<'source> {
    source: Peekable<Chars<'source>>,
    ln: usize,
    col: usize,
}
#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
    ExpectedEscape,
    ExpectedCharacter,
    UnclosedCharacter,
    UnclosedString,
    BadCharacter(char),
    ParseIntError(ParseIntError),
    ParseFloatError(ParseFloatError),
}
impl<'source> Lexer<'source> {
    pub fn advance(&mut self) {
        if self.source.peek() == Some(&'\n') {
            self.ln += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }
    pub fn pos(&self) -> Position {
        Position::single(self.ln, self.col)
    }
    pub fn lex(&mut self) -> Result<Vec<Located<Token>>, Located<LexError>> {
        let mut tokens = vec![];
        for res in self.by_ref() {
            tokens.push(res?);
        }
        Ok(tokens)
    }
}
impl<'source> Iterator for Lexer<'source> {
    type Item = Result<Located<Token>, Located<LexError>>;
    fn next(&mut self) -> Option<Self::Item> {
        while self.source.next_if(|c| c.is_ascii_whitespace()).is_some() {
            self.advance();
        }
        let mut pos = self.pos();
        self.advance();
        match self.source.next()? {
            '=' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::EqualEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Equal, pos)))
                }
            }
            ',' => Some(Ok(Located::new(Token::Comma, pos))),
            '.' => Some(Ok(Located::new(Token::Dot, pos))),
            '!' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::ExclamationEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Exclamation, pos)))
                }
            }
            '(' => Some(Ok(Located::new(Token::ParanLeft, pos))),
            ')' => Some(Ok(Located::new(Token::ParanRight, pos))),
            '[' => Some(Ok(Located::new(Token::BracketLeft, pos))),
            ']' => Some(Ok(Located::new(Token::BracketRight, pos))),
            '{' => Some(Ok(Located::new(Token::BraceLeft, pos))),
            '}' => Some(Ok(Located::new(Token::BraceRight, pos))),
            '+' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::PlusEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Plus, pos)))
                }
            }
            '-' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::MinusEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Minus, pos)))
                }
            }
            '*' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::StarEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Star, pos)))
                }
            }
            '/' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::SlashEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Slash, pos)))
                }
            }
            '%' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::PercentEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Percent, pos)))
                }
            }
            '^' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::ExponentEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Exponent, pos)))
                }
            }
            '<' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::LessEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Less, pos)))
                }
            }
            '>' => {
                if self.source.peek() == Some(&'=') {
                    self.source.next();
                    pos.extend(&self.pos());
                    self.advance();
                    Some(Ok(Located::new(Token::GreaterEqual, pos)))
                } else {
                    Some(Ok(Located::new(Token::Greater, pos)))
                }
            }
            '&' => Some(Ok(Located::new(Token::Ampersand, pos))),
            '|' => Some(Ok(Located::new(Token::Pipe, pos))),
            '\'' => {
                let c = match self
                    .source
                    .next()
                    .ok_or(LexError::ExpectedCharacter)
                    .map_err(|err| Located::new(err, self.pos()))
                {
                    Ok(c) => match c {
                        '\\' => {
                            self.advance();
                            let c = match self.source.peek() {
                                Some('n') => '\n',
                                Some('t') => '\t',
                                Some('r') => '\r',
                                Some('0') => '\0',
                                Some(c) => *c,
                                None => {
                                    return Some(Err(Located::new(
                                        LexError::ExpectedEscape,
                                        self.pos(),
                                    )))
                                }
                            };
                            self.source.next();
                            c
                        }
                        c => c,
                    },
                    Err(err) => return Some(Err(err)),
                };
                self.advance();
                if self.source.next_if(|c| *c == '\'').is_none() {
                    return Some(Err(Located::new(LexError::UnclosedCharacter, pos)));
                }
                Some(Ok(Located::new(Token::Char(c), pos)))
            }
            '"' => {
                let mut string = String::new();
                while let Some(c) = self.source.peek() {
                    if *c == '"' {
                        break;
                    }
                    string.push(match *c {
                        '\\' => {
                            self.source.next()?;
                            self.advance();
                            match self.source.peek() {
                                Some('n') => '\n',
                                Some('t') => '\t',
                                Some('r') => '\r',
                                Some('0') => '\0',
                                Some(c) => *c,
                                None => {
                                    return Some(Err(Located::new(
                                        LexError::ExpectedEscape,
                                        self.pos(),
                                    )))
                                }
                            }
                        }
                        c => c,
                    });
                    pos.extend(&self.pos());
                    self.advance();
                    self.source.next();
                }
                if self.source.next_if(|c| *c == '"').is_none() {
                    return Some(Err(Located::new(LexError::UnclosedString, pos)));
                }
                Some(Ok(Located::new(Token::String(string), pos)))
            }
            c if c.is_ascii_digit() => {
                let mut number = String::from(c);
                while let Some(c) = self.source.peek() {
                    if !c.is_ascii_alphanumeric() && *c != '_' {
                        break;
                    }
                    number.push(*c);
                    pos.extend(&self.pos());
                    self.advance();
                    self.source.next();
                }
                if self.source.next_if(|c| *c == '.').is_some() {
                    number.push('.');
                    pos.extend(&self.pos());
                    self.advance();
                    while let Some(c) = self.source.peek() {
                        if !c.is_ascii_alphanumeric() && *c != '_' {
                            break;
                        }
                        number.push(*c);
                        pos.extend(&self.pos());
                        self.advance();
                        self.source.next();
                    }
                    Some(Ok(Located::new(
                        Token::Float(
                            match number
                                .parse()
                                .map_err(LexError::ParseFloatError)
                                .map_err(|err| Located::new(err, pos.clone()))
                            {
                                Ok(number) => number,
                                Err(err) => return Some(Err(err)),
                            },
                        ),
                        pos,
                    )))
                } else {
                    Some(Ok(Located::new(
                        Token::Int(
                            match number
                                .parse()
                                .map_err(LexError::ParseIntError)
                                .map_err(|err| Located::new(err, pos.clone()))
                            {
                                Ok(number) => number,
                                Err(err) => return Some(Err(err)),
                            },
                        ),
                        pos,
                    )))
                }
            }
            c if c.is_ascii_alphanumeric() || c == '_' => {
                let mut ident = String::from(c);
                while let Some(c) = self.source.peek() {
                    if !c.is_ascii_alphanumeric() && *c != '_' {
                        break;
                    }
                    ident.push(*c);
                    pos.extend(&self.pos());
                    self.advance();
                    self.source.next();
                }
                Some(Ok(Located::new(Token::ident(ident), pos)))
            }
            c => Some(Err(Located::new(LexError::BadCharacter(c), pos))),
        }
    }
}
impl<'source> From<&'source str> for Lexer<'source> {
    fn from(value: &'source str) -> Self {
        Self {
            source: value.chars().peekable(),
            ln: 0,
            col: 0,
        }
    }
}
impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::ExpectedEscape => write!(f, "expected escape character"),
            LexError::ExpectedCharacter => write!(f, "expected a character"),
            LexError::UnclosedCharacter => write!(f, "unclosed character"),
            LexError::UnclosedString => write!(f, "unclosed string"),
            LexError::BadCharacter(c) => write!(f, "bad character {c:?}"),
            LexError::ParseIntError(err) => write!(f, "error while parsing to int: {err}"),
            LexError::ParseFloatError(err) => write!(f, "error while parsing to float: {err}"),
        }
    }
}
impl Error for LexError {}
