pub mod lang;
pub mod luna_impl;
#[cfg(test)]
pub mod tests;
use lang::{
    code::Closure,
    value::{Function, Value},
    interpreter::Interpreter,
};
use luna_impl::{
    compiler::{Compilable, Compiler},
    lexer::Lexer,
    parser::{Parsable, ParseError},
    position::{Located, PathLocated},
    tokens::Token,
};
use std::{cell::RefCell, env, error::Error, fmt::Display, rc::Rc};

pub fn lex_str(text: &str) -> Result<Vec<Located<Token>>, Located<Box<dyn Error>>> {
    Lexer::from(text)
        .lex()
        .map_err(|err| err.map(|err| err.into()))
}
pub fn parse_str<A: Parsable>(text: &str) -> Result<Located<A>, Located<Box<dyn Error>>> {
    let mut lexer = lex_str(text)?.into_iter().peekable();
    let ast = A::parse(&mut lexer).map_err(|err| err.map(|err| err.into()))?;
    if let Some(Located { value: token, pos }) = lexer.next() {
        return Err(Located::new(ParseError::UnexpectedToken(token).into(), pos));
    }
    Ok(ast)
}
pub fn compile_str<A: Parsable>(
    text: &str,
    path: Option<&str>,
) -> Result<Rc<RefCell<Closure>>, Located<Box<dyn Error>>>
where
    Located<A>: Compilable<Output = Rc<RefCell<Closure>>>,
{
    let ast = parse_str::<A>(text)?;
    let closure = ast.compile(&mut Compiler {
        path: path.map(|s| s.to_string()),
        ..Default::default()
    })?;
    Ok(closure)
}
pub fn run_str<A: Parsable>(
    text: &str,
    path: Option<&str>,
) -> Result<Option<Value>, PathLocated<Box<dyn Error>>>
where
    Located<A>: Compilable<Output = Rc<RefCell<Closure>>>,
{
    let closure = compile_str(text, path).map_err(|err| {
        err.with_path(
            path.map(|path| path.to_string())
                .unwrap_or("<input>".to_string()),
        )
    })?;
    let function = Rc::new(RefCell::new(Function {
        closure,
        upvalues: vec![],
        meta: None
    }));
    let mut interpreter = Interpreter::default().with_global_path(env::var("LUNA_PATH").ok());
    interpreter.call(&function, vec![], None);
    interpreter.run().map_err(|err| {
        err.map(|err| err.into())
            .with_path(interpreter.path().unwrap_or("<input>".to_string()))
    })
}

#[macro_export]
macro_rules! int {
    ($v:literal) => {
        Value::Int($v.into())
    };
}
#[macro_export]
macro_rules! float {
    ($v:literal) => {
        Value::Float($v.into())
    };
}
#[macro_export]
macro_rules! bool {
    ($v:literal) => {
        Value::Bool($v.into())
    };
}
#[macro_export]
macro_rules! char {
    ($v:literal) => {
        Value::Char($v.into())
    };
}
#[macro_export]
macro_rules! string {
    ($v:literal) => {
        Value::String($v.to_string())
    };
}
#[macro_export]
macro_rules! vector {
    [$($v:literal),*] => {
        Value::Vector(Rc::new(RefCell::new(vec![$($v.into()),*])))
    };
    ($v:expr) => {
        Value::Vector(Rc::new(RefCell::new($v.into())))
    };
}
#[macro_export]
macro_rules! object {
    {$($k:literal = $v:expr),*} => {
        {
            #[allow(unused_variables, unused_mut)]
            let mut map = HashMap::new();
            $(
                map.insert($k.into(), $v.into());
            ) *
            Value::Object(Rc::new(RefCell::new(Object::new(map))))
        }
    };
    ($v:expr) => {
        Value::Object(Rc::new(RefCell::new(Object::new($v))))
    };
}
#[macro_export]
macro_rules! function {
    ($name:ident) => {
        Value::Function(FunctionKind::UserFunction(Rc::new($name)))
    };
    ($name:path) => {
        Value::Function(FunctionKind::UserFunction(Rc::new($name)))
    };
}
#[macro_export]
macro_rules! set_field {
    ($map:ident . $field:literal = $value:expr) => {
        $map.insert($field.to_string(), Rc::new(RefCell::new($value)));
    };
    ($map:ident . $field:ident = $value:expr) => {
        $map.insert($field.to_string(), Rc::new(RefCell::new($value)));
    };
}
#[derive(Debug, Clone, PartialEq)]
pub struct ExpectedType {
    pub idx: usize,
    pub expected: &'static str,
    pub got: &'static str,
}
impl Display for ExpectedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected {} for argument #{}, got {}",
            self.expected, self.idx, self.got
        )
    }
}
impl Error for ExpectedType {}
#[derive(Debug, Clone, PartialEq)]
pub struct ExpectedTypes {
    pub idx: usize,
    pub expected: Vec<&'static str>,
    pub got: &'static str,
}
impl Display for ExpectedTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected {} for argument #{}, got {}",
            self.expected.join("/"),
            self.idx,
            self.got
        )
    }
}
impl Error for ExpectedTypes {}
#[macro_export]
macro_rules! typed {
    ($args:ident) => {{
        let (_, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        arg
    }};
    ($args:ident : $type:ident) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if let Value::$type(value) = arg {
            value
        } else {
            return Err(ExpectedType {
                idx,
                expected: Value::$type(Default::default()).typ(),
                got: arg.typ(),
            }
            .into());
        }
    }};
    ($args:ident : $type:ident ?) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if arg == Value::default() {
            None
        } else {
            if let Value::$type(value) = arg {
                Some(value)
            } else {
                return Err(ExpectedType {
                    idx,
                    expected: Value::$type(Default::default()).typ(),
                    got: arg.typ(),
                }
                .into());
            }
        }
    }};
    ($args:ident : $type:ident $param:ident => $value:expr) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if let Value::$type($param) = arg {
            $value
        } else {
            return Err(ExpectedType {
                idx,
                expected: Value::$type(Default::default()).typ(),
                got: arg.typ(),
            }
            .into());
        }
    }};
    ($args:ident : $type:ident ? $param:ident => $value:expr) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        if arg == Value::default() {
            None
        } else {
            if let Value::$type($param) = arg {
                Some($value)
            } else {
                return Err(ExpectedType {
                    idx,
                    expected: Value::$type(Default::default()).typ(),
                    got: arg.typ(),
                }
                .into());
            }
        }
    }};
}
#[macro_export]
macro_rules! option {
    ($args:ident : $($type:ident => $value:ident $body:block),+) => {{
        let (idx, arg) = $args.next().unwrap_or(($args.len(), Value::default()));
        match arg {
            $(
                Value::$type($value) => $body,
            ) +
            arg => {
                return Err(ExpectedTypes {
                    idx,
                    expected: vec![$(Value::$type(Default::default()).typ()),+],
                    got: arg.typ(),
                }
                .into())
            }
        }
    }};
}
#[macro_export]
macro_rules! userobject {
    (
        $name:ident : $typ_name:literal ;
        $self:ident
        $(yield { $(
            $key_literal_name:literal = $key_expression:expr
        ) *})?
        $(static ($fn_self:ident, $fn_interpreter:ident, $fn_args:ident) { $(
            $fn_name:ident : $fn_literal_name:literal $fn_body:block
        ) *})?
        $(mut ($fn_mut_self:ident, $fn_mut_interpreter:ident, $fn_mut_args:ident) { $(
            $fn_mut_name:ident : $fn_mut_literal_name:literal $fn_mut_body:block
        ) *})?
    ) => {
        impl UserObject for $name {
            fn typ(&self) -> &'static str {
                $typ_name
            }
            fn get(&$self, key: &str) -> Option<Value> {
                match key {
                    $(
                        $(
                            $key_literal_name => Some($key_expression),
                        )*
                    )?
                    $(
                        $(
                            $fn_literal_name => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                                Box::new(Self::$fn_name),
                            )))),
                        )*
                    )?
                    $(
                        $(
                            $fn_mut_literal_name => Some(Value::Function(FunctionKind::UserFunction(Rc::new(
                                Box::new(Self::$fn_mut_name),
                            )))),
                        )*
                    )?
                    _ => None,
                }
            }
            $(
                #[allow(unused_variables)]
                fn call(&$fn_self, key: &str, $fn_interpreter: &mut Interpreter, $fn_args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
                    match key {
                        $(
                            $fn_literal_name => $fn_body,
                        )+
                        _ => Err(Box::new(UserObjectError::CannotCallNull))
                    }
                }
            )?
            $(
                #[allow(unused_variables)]
                fn call_mut(&mut $fn_mut_self, key: &str, $fn_mut_interpreter: &mut Interpreter, $fn_mut_args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
                    match key {
                        $(
                            $fn_mut_literal_name => $fn_mut_body,
                        )+
                        _ => Err(Box::new(UserObjectError::CannotCallNull))
                    }
                }
            )?
        }
        impl $name {
            $(
                $(
                    pub fn $fn_name($fn_interpreter: &mut Interpreter, mut $fn_args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
                        let Some(_self) = $fn_args.first().cloned() else {
                            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
                        };
                        $fn_args.remove(0);
                        if let Value::UserObject(_self) = _self {
                            let mut _self = _self.borrow_mut();
                            _self.call($fn_literal_name, $fn_interpreter, $fn_args)
                        } else {
                            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
                        }
                    }
                ) *
            )?
            $(
                $(
                    pub fn $fn_mut_name($fn_mut_interpreter: &mut Interpreter, mut $fn_mut_args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
                        let Some(_self) = $fn_mut_args.first().cloned() else {
                            return Err(Box::new(UserObjectError::ExpectedSelf("null")));
                        };
                        $fn_mut_args.remove(0);
                        if let Value::UserObject(_self) = _self {
                            let mut _self = _self.borrow_mut();
                            _self.call_mut($fn_mut_literal_name, $fn_mut_interpreter, $fn_mut_args)
                        } else {
                            Err(Box::new(UserObjectError::ExpectedSelf(_self.typ())))
                        }
                    }
                ) *
            )?
        }
    };
}
