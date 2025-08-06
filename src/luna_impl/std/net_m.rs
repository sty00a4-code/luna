use crate::{
    function, lang::{
        interpreter::Interpreter,
        value::{FunctionKind, UserObject, UserObjectError, Value, Object},
    }, object, set_field, typed, userobject, ExpectedType
};
use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    io::{Read, Write},
    net::{TcpListener, TcpStream},
    rc::Rc,
};

pub fn define(globals: &mut HashMap<String, Rc<RefCell<Value>>>) {
    set_field!(globals."net" = object! {
        "bind" = function!(_bind),
        "connect" = function!(_connect)
    });
}

pub fn _bind(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let addr = typed!(args: String);
    let port = typed!(args: Int port => u16::try_from(port))
        .map_err(|_| Into::<Box<dyn Error>>::into("invalid port"))?;
    Ok(TcpListener::bind((addr, port))
        .map(|listener| {
            Value::UserObject(Rc::new(RefCell::new(Box::new(TcpListenerObject(listener)))))
        })
        .unwrap_or_default())
}
pub fn _connect(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let addr = typed!(args: String);
    let port = typed!(args: Int port => u16::try_from(port))
        .map_err(|_| Into::<Box<dyn Error>>::into("invalid port"))?;
    Ok(TcpStream::connect((addr, port))
        .map(|stream| Value::UserObject(Rc::new(RefCell::new(Box::new(TcpStreamObject(stream))))))
        .unwrap_or_default())
}

#[derive(Debug)]
pub struct TcpListenerObject(TcpListener);
userobject! {
    TcpListenerObject: "tcp-listener";
    self
    static (self, _i, args) {
        addr : "addr"  {
            Ok(self
                .0
                .local_addr()
                .map(|addr| Value::String(addr.to_string()))
                .unwrap_or_default())
        }
    }
    mut (self, _i, args) {
        accept : "accept" {
            Ok(self
                .0
                .accept()
                .map(|(stream, _)| {
                    Value::UserObject(Rc::new(RefCell::new(Box::new(TcpStreamObject(stream)))))
                })
                .unwrap_or_default())
        }
    }
}
#[derive(Debug)]
pub struct TcpStreamObject(TcpStream);
userobject! {
    TcpStreamObject: "tcp-stream";
    self
    static (self, _i, _args) {
        local_addr : "local_addr" {
            Ok(self
                .0
                .local_addr()
                .map(|addr| Value::String(addr.to_string()))
                .unwrap_or_default())
        }
        peer_addr : "peer_addr" {
            Ok(self
                .0
                .peer_addr()
                .map(|addr| Value::String(addr.to_string()))
                .unwrap_or_default())
        }
    }
    mut (self, _i, args) {
        read : "read" {
            let mut buf = String::new();
            let Ok(_) = self.0.read_to_string(&mut buf) else {
                return Ok(Value::default());
            };
            Ok(Value::String(buf))
        }
        write : "write"  {
            let mut args = args.into_iter().enumerate();
            let message = typed!(args: String);
            Ok(self.0.write(message.as_bytes()).unwrap_or_default().into())
        }
        flush : "flush"  {
            self.0.flush()?;
            Ok(Value::default())
        }
    }
}
