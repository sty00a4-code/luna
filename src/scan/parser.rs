use super::position::{Located, Position};
use crate::lang::{ast::*, tokens::Token};
use std::{error::Error, fmt::Display, iter::Peekable, vec::IntoIter};

pub type Parser = Peekable<IntoIter<Located<Token>>>;
pub trait Parsable
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>>;
}
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedEOF,
    UnexpectedToken(Token),
    ExpectedToken { expected: Token, got: Token },
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedEOF => write!(f, "unexpected end of file"),
            ParseError::UnexpectedToken(token) => write!(f, "unexpected {}", token.name()),
            ParseError::ExpectedToken { expected, got } => {
                write!(f, "expected {}, got {}", expected.name(), got.name())
            }
        }
    }
}
impl Error for ParseError {}

impl Parsable for Chunk {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let mut stats = vec![];
        let mut pos = Position::default();
        while parser.peek().is_some() {
            let stat = Statement::parse(parser)?;
            pos.extend(&stat.pos);
            stats.push(stat);
        }
        Ok(Located::new(Self(stats), pos))
    }
}
impl Parsable for Block {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let Some(Located {
            value: start_token,
            mut pos,
        }) = parser.next()
        else {
            return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
        };
        if start_token != Token::BraceLeft {
            return Err(Located::new(
                ParseError::ExpectedToken {
                    expected: Token::BraceLeft,
                    got: start_token,
                },
                pos,
            ));
        }
        let mut stats = vec![];
        while parser.peek().is_some() {
            if let Some(Located {
                value: Token::BraceRight,
                pos: _,
            }) = parser.peek()
            {
                break;
            }
            let stat = Statement::parse(parser)?;
            pos.extend(&stat.pos);
            stats.push(stat);
        }
        let Some(Located {
            value: end_token,
            pos: end_pos,
        }) = parser.next()
        else {
            return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
        };
        if end_token == Token::BraceRight {
            pos.extend(&end_pos);
            Ok(Located::new(Self(stats), pos))
        } else {
            Err(Located::new(
                ParseError::ExpectedToken {
                    expected: Token::BraceRight,
                    got: end_token,
                },
                end_pos,
            ))
        }
    }
}
impl Parsable for Statement {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let Some(Located { value: token, pos }) = parser.peek() else {
            return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
        };
        match token {
            Token::BraceLeft => Ok(Block::parse(parser)?.map(Self::Block)),
            Token::Let => {
                let Located { value: _, mut pos } = parser.next().unwrap();
                let mut idents = vec![];
                let mut exprs = vec![];
                while matches!(
                    parser.peek(),
                    Some(Located {
                        value: Token::Ident(_),
                        pos: _
                    })
                ) {
                    let ident = Path::ident(parser)?;
                    pos.extend(&ident.pos);
                    idents.push(ident);
                    if matches!(
                        parser.peek(),
                        Some(Located {
                            value: Token::Comma,
                            pos: _
                        })
                    ) {
                        parser.next();
                    } else if matches!(
                        parser.peek(),
                        Some(Located {
                            value: Token::Equal,
                            pos: _
                        })
                    ) {
                        break;
                    }
                }
                let Some(Located {
                    value: token,
                    pos: token_pos,
                }) = parser.next()
                else {
                    return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
                };
                if token != Token::Equal {
                    return Err(Located::new(
                        ParseError::ExpectedToken {
                            expected: Token::Equal,
                            got: token,
                        },
                        token_pos,
                    ));
                }
                let expr = Expression::parse(parser)?;
                pos.extend(&expr.pos);
                exprs.push(expr);
                while matches!(
                    parser.peek(),
                    Some(Located {
                        value: Token::Comma,
                        pos: _
                    })
                ) {
                    parser.next();
                    let expr = Expression::parse(parser)?;
                    pos.extend(&expr.pos);
                    exprs.push(expr);
                }
                Ok(Located::new(Self::LetBinding { idents, exprs }, pos))
            }
            _ => Err(Located::new(
                ParseError::UnexpectedToken(token.clone()),
                pos.clone(),
            )),
        }
    }
}
impl Parsable for Expression {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        Self::binary(parser, 0)
    }
}
impl Expression {
    pub fn binary(parser: &mut Parser, layer: usize) -> Result<Located<Self>, Located<ParseError>> {
        let Some(ops) = BinaryOperator::layer(layer) else {
            return Self::unary(parser, 0);
        };
        let mut left = Self::binary(parser, layer + 1)?;
        while let Some(Located {
            value: token,
            pos: _,
        }) = parser.peek()
        {
            let Ok(op) = BinaryOperator::try_from(token) else {
                break;
            };
            if !ops.contains(&op) {
                break;
            }
            parser.next();
            let right = Self::binary(parser, layer + 1)?;
            let mut pos = left.pos.clone();
            pos.extend(&right.pos);
            left = Located::new(
                Self::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                pos,
            );
        }
        Ok(left)
    }
    pub fn unary(parser: &mut Parser, layer: usize) -> Result<Located<Self>, Located<ParseError>> {
        let Some(ops) = UnaryOperator::layer(layer) else {
            return Self::atom(parser);
        };
        if let Some(Located {
            value: token,
            pos: _,
        }) = parser.peek()
        {
            if let Ok(op) = UnaryOperator::try_from(token) {
                if ops.contains(&op) {
                    let Located { value: _, mut pos } = parser.next().unwrap();
                    let right = Self::unary(parser, layer)?;
                    pos.extend(&right.pos);
                    return Ok(Located::new(
                        Self::Unary {
                            op,
                            right: Box::new(right),
                        },
                        pos,
                    ));
                }
            }
        }
        Self::unary(parser, layer + 1)
    }
    pub fn atom(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        Ok(Atom::parse(parser)?.map(Self::Atom))
    }
}
impl Parsable for Path {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        let mut head = Self::ident(parser)?.map(Self::Ident);
        while let Some(Located {
            value: token,
            pos: _,
        }) = parser.peek()
        {
            match token {
                Token::Dot => {
                    parser.next();
                    let field = Self::ident(parser)?;
                    let mut pos = head.pos.clone();
                    pos.extend(&field.pos);
                    head = Located::new(
                        Self::Field {
                            head: Box::new(head),
                            field,
                        },
                        pos,
                    )
                }
                Token::BracketLeft => {
                    parser.next();
                    let index = Expression::parse(parser)?;
                    let Some(Located {
                        value: token,
                        pos: end_pos,
                    }) = parser.next()
                    else {
                        return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
                    };
                    if token != Token::BracketRight {
                        return Err(Located::new(
                            ParseError::ExpectedToken {
                                expected: Token::BracketRight,
                                got: token,
                            },
                            end_pos,
                        ));
                    }
                    let mut pos = head.pos.clone();
                    pos.extend(&end_pos);
                    head = Located::new(
                        Self::Index {
                            head: Box::new(head),
                            index: Box::new(index),
                        },
                        pos,
                    )
                }
                _ => break,
            }
        }
        Ok(head)
    }
}
impl Path {
    pub fn ident(parser: &mut Parser) -> Result<Located<String>, Located<ParseError>> {
        let Some(Located { value: token, pos }) = parser.next() else {
            return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
        };
        if let Token::Ident(ident) = token {
            Ok(Located::new(ident, pos))
        } else {
            Err(Located::new(
                ParseError::ExpectedToken {
                    expected: Token::Ident(Default::default()),
                    got: token,
                },
                pos,
            ))
        }
    }
}
impl Parsable for Atom {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Located<ParseError>> {
        if let Some(Located {
            value: Token::Ident(_),
            pos: _,
        }) = parser.peek()
        {
            return Ok(Path::parse(parser)?.map(Self::Path));
        }
        let Some(Located {
            value: token,
            mut pos,
        }) = parser.next()
        else {
            return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
        };
        match token {
            Token::Null => Ok(Located::new(Self::Null, pos)),
            Token::Int(v) => Ok(Located::new(Self::Int(v), pos)),
            Token::Float(v) => Ok(Located::new(Self::Float(v), pos)),
            Token::Bool(v) => Ok(Located::new(Self::Bool(v), pos)),
            Token::Char(v) => Ok(Located::new(Self::Char(v), pos)),
            Token::String(v) => Ok(Located::new(Self::String(v), pos)),
            Token::ParanLeft => {
                let expr = Expression::parse(parser)?;
                let Some(Located {
                    value: end_token,
                    pos: end_pos,
                }) = parser.next()
                else {
                    return Err(Located::new(ParseError::UnexpectedEOF, Position::default()));
                };
                if end_token != Token::ParanRight {
                    return Err(Located::new(
                        ParseError::ExpectedToken {
                            expected: Token::ParanRight,
                            got: end_token,
                        },
                        end_pos,
                    ));
                }
                pos.extend(&end_pos);
                Ok(Located::new(Self::Expression(Box::new(expr)), pos))
            }
            token => Err(Located::new(ParseError::UnexpectedToken(token), pos)),
        }
    }
}
