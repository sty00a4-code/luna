use crate::{
    lang::{
        interpreter::Interpreter,
        value::{FunctionKind, UserObject, UserObjectError, Value},
    },
    typed, userobject, ExpectedType,
};
use std::{
    cell::RefCell,
    error::Error,
    fmt::Display,
    fs::{self, File},
    io::{Read, Write},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq)]
pub struct InvalidOptionError(String);
impl Display for InvalidOptionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid option {:?}", self.0)
    }
}
impl Error for InvalidOptionError {}
pub fn _open(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let path = typed!(args: String);
    let (read, write, append) = typed!(args: String options => match options.as_str() {
        "w" => (true, true, false),
        "r" => (true, false, false),
        "a" => (true, false, true),
        _ => return Err(InvalidOptionError(options).into())
    });
    Ok(File::options()
        .read(read)
        .write(write)
        .create(write)
        .append(append)
        .open(path)
        .map(|file| Value::UserObject(Rc::new(RefCell::new(Box::new(FileObject(file))))))
        .unwrap_or_default())
}
pub fn _list(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let path = typed!(args: String);
    Ok(fs::read_dir(path)?
        .flatten()
        .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
        .collect::<Vec<String>>()
        .into())
}
pub fn _type(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    let mut args = args.into_iter().enumerate();
    let path = typed!(args: String);
    if fs::metadata(&path)?.is_dir() {
        Ok(Value::String("dir".to_string()))
    } else if fs::metadata(&path)?.is_file() {
        Ok(Value::String("file".to_string()))
    } else {
        Ok(Value::default())
    }
}

#[derive(Debug)]
pub struct FileObject(File);
userobject! {
    FileObject: "file";
    self
    mut (self, _i, args) {
        read : "read" {
            let mut string = String::new();
            self.0.read_to_string(&mut string)?;
            Ok(Value::String(string))
        }
        write : "write"  {
            let mut args = args.into_iter().enumerate();
            let buf = typed!(args: String);
            Ok(Value::Int(self.0.write(&buf.into_bytes())? as i64))
        }
    }
}
