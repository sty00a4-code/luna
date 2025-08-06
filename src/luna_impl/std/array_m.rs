use crate::{
    function,
    lang::{
        interpreter::Interpreter,
        value::{FunctionKind, Object, UserObject, UserObjectError, Value},
    },
    object, set_field, typed, userobject, ExpectedType,
};
use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, rc::Rc, vec::IntoIter};

pub fn define(globals: &mut HashMap<String, Rc<RefCell<Value>>>) {
    set_field!(globals."array" = object! {
        "u8" = function!(ArrayU8::_new),
        "u16" = function!(ArrayU16::_new),
        "u32" = function!(ArrayU32::_new),
        "u64" = function!(ArrayU64::_new),
        "u128" = function!(ArrayU128::_new),
        "i8" = function!(ArrayI8::_new),
        "i16" = function!(ArrayI16::_new),
        "i32" = function!(ArrayI32::_new),
        "i64" = function!(ArrayI64::_new),
        "i128" = function!(ArrayI128::_new),
        "f32" = function!(ArrayF32::_new),
        "f64" = function!(ArrayF64::_new),
        "bool" = function!(ArrayBool::_new),
        "char" = function!(ArrayChar::_new)
    });
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayError {
    InvalidElementType(String),
}
impl Display for ArrayError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArrayError::InvalidElementType(typ) => write!(f, "invalid element type {typ:?}"),
        }
    }
}
impl Error for ArrayError {}

macro_rules! define_int_array {
    ($name:ident $typ:ty = $luna_typ:ident : $type_name:literal; $iterator_name:ident : $iterator_type_name:literal) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            pub array: Vec<$typ>,
        }
        pub struct $iterator_name(pub IntoIter<$typ>);
        userobject! {
            $iterator_name : $iterator_type_name;
            self
            mut (self, interpreter, args) {
                next : "next" {
                    Ok(self.0.next().map(|v| v.cast().map(Value::$luna_typ).unwrap_or_default()).unwrap_or_default())
                }
                any : "any" {
                    let mut args = args.into_iter().enumerate();
                    let func = typed!(args: Function);
                    match func {
                        FunctionKind::Function(func) => {
                            for v in self.0.by_ref() {
                                interpreter.call(&func, vec![v.into()], None);
                                let res = interpreter
                                    .run()
                                    .map_err(|err| Box::new(err.value))?
                                    .unwrap_or_default();
                                if bool::from(res) {
                                    return Ok(true.into());
                                }
                            }
                            Ok(false.into())
                        }
                        FunctionKind::UserFunction(func) => {
                            for v in self.0.by_ref() {
                                let res = func(interpreter, vec![v.into()])?;
                                if bool::from(res) {
                                    return Ok(true.into());
                                }
                            }
                            Ok(false.into())
                        }
                    }
                }
                all : "all" {
                    let mut args = args.into_iter().enumerate();
                    let func = typed!(args: Function);
                    match func {
                        FunctionKind::Function(func) => {
                            for v in self.0.by_ref() {
                                interpreter.call(&func, vec![v.into()], None);
                                let res = interpreter
                                    .run()
                                    .map_err(|err| Box::new(err.value))?
                                    .unwrap_or_default();
                                if !bool::from(res) {
                                    return Ok(false.into());
                                }
                            }
                            Ok(true.into())
                        }
                        FunctionKind::UserFunction(func) => {
                            for v in self.0.by_ref() {
                                let res = func(interpreter, vec![v.into()])?;
                                if !bool::from(res) {
                                    return Ok(false.into());
                                }
                            }
                            Ok(true.into())
                        }
                    }
                }
                collect : "collect" {
                    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new($name {
                            array: self.0.clone().map(|v| v.into()).collect::<Vec<$typ>>()
                    })))))
                }
            }
        }
        userobject! {
            $name : $type_name;
            self
            static (self, _i, args) {
                iter : "iter" {
                    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                        $iterator_name(self.array.clone().into_iter()),
                    )))))
                }
                get : "get" {
                    let mut args = args.into_iter().enumerate();
                    let index = typed!(args: Int);
                    let default = args.next().map(|(_, v)| v);
                    Ok(self.array.get(index as usize).map(|v| Value::Int(*v as i64)).unwrap_or(default.unwrap_or_default()))
                }
                len : "len" {
                    Ok(self.array.len().into())
                }
                contains : "contains" {
                    let mut args = args.into_iter().enumerate();
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    Ok(self.array.contains(&value).into())
                }
                position : "position" {
                    let mut args = args.into_iter().enumerate();
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    Ok(self.array
                        .iter()
                        .position(|v| v == &value)
                        .map(|pos| pos.into())
                        .unwrap_or_default())
                }
                count : "count" {
                    let mut args = args.into_iter().enumerate();
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    Ok(Value::Int(self.array
                        .iter()
                        .filter(|v| **v == value)
                        .count() as i64))
                }
                join : "join" {
                    let mut args = args.into_iter().enumerate();
                    let sep = typed!(args: String);
                    Ok(Value::String(
                        self.array
                            .iter()
                            .map(|v| v.to_string())
                            .collect::<Vec<String>>()
                            .join(&sep),
                    ))
                }
                copy : "copy" {
                    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                        Self { array: self.array.clone() },
                    )))))
                }
            }
            mut (self, _i, args) {
                set : "set" {
                    let mut args = args.into_iter().enumerate();
                    let index = typed!(args: Int);
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    if let Some(old_value) = self.array.get_mut(index as usize) {
                        *old_value = value;
                    }
                    Ok(Value::default())
                }
                push : "push" {
                    let mut args = args.into_iter().enumerate();
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    self.array.push(value);
                    Ok(Value::default())
                }
                pop : "pop" {
                    let mut args = args.into_iter().enumerate();
                    let index = typed!(args: Int?);

                    if let Some(index) = index {
                        let index = index.unsigned_abs() as usize;
                        if self.array.get(index).is_some() {
                            Ok(Value::Int(self.array.remove(index) as i64))
                        } else {
                            Ok(Value::default())
                        }
                    } else {
                        Ok(self.array.pop().map(|v| Value::Int(v as i64)).unwrap_or_default())
                    }
                }
                insert : "insert" {
                    let mut args = args.into_iter().enumerate();
                    let index = typed!(args: Int int => int.unsigned_abs() as usize);
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    if index <= self.array.len() {
                        self.array.insert(index, value);
                    }
                    Ok(Value::default())
                }
                swap : "swap" {
                    let mut args = args.into_iter().enumerate();
                    let index1 = typed!(args: Int int => int.unsigned_abs() as usize);
                    let index2 = typed!(args: Int int => int.unsigned_abs() as usize);
                    if self.array.get(index1).is_some() && self.array.get(index2).is_some() {
                        self.array.swap(index1, index2);
                    }
                    Ok(Value::default())
                }
                clear : "clear" {
                    self.array.clear();
                    Ok(Value::default())
                }
                sort : "sort" {
                    self.array.sort();
                    Ok(Value::default())
                }
                max : "max" {
                    Ok(Value::Int(self.array.iter().max().cloned().unwrap_or_default() as i64))
                }
                min : "min" {
                    Ok(Value::Int(self.array.iter().min().cloned().unwrap_or_default() as i64))
                }
            }
        }
        impl $name {
            pub fn _new(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
                let mut args = args.into_iter().enumerate();
                let length = typed!(args: Int? int => int as usize);
                let mut array = typed!(args: Vector? vector => {
                    let mut array = Vec::with_capacity(length.unwrap_or(1));
                    for value in vector.borrow().iter() {
                        array.push(Self::try_cast(value)?);
                    }
                    array
                })
                .unwrap_or_default();
                if let Some(length) = length {
                    while array.len() < length {
                        array.push(Default::default());
                    }
                }
                Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                    Self { array },
                )))))
            }
            pub fn try_cast(value: &Value) -> Result<$typ, Box<dyn Error>> {
                match value {
                    Value::$luna_typ(v) => Ok((*v).cast()?),
                    value => Err(Box::new(ArrayError::InvalidElementType(
                        value.dynamic_typ(),
                    ))),
                }
            }
        }
    };
}
macro_rules! define_array {
    ($name:ident $typ:ty = $luna_typ:ident : $type_name:literal; $iterator_name:ident : $iterator_type_name:literal) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            pub array: Vec<$typ>,
        }
        pub struct $iterator_name(pub IntoIter<$typ>);
        userobject! {
            $iterator_name : $iterator_type_name;
            self
            mut (self, interpreter, args) {
                next : "next" {
                    Ok(self.0.next().map(|v| v.cast().map(Value::$luna_typ).unwrap_or_default()).unwrap_or_default())
                }
                any : "any" {
                    let mut args = args.into_iter().enumerate();
                    let func = typed!(args: Function);
                    match func {
                        FunctionKind::Function(func) => {
                            for v in self.0.by_ref() {
                                interpreter.call(&func, vec![v.into()], None);
                                let res = interpreter
                                    .run()
                                    .map_err(|err| Box::new(err.value))?
                                    .unwrap_or_default();
                                if bool::from(res) {
                                    return Ok(true.into());
                                }
                            }
                            Ok(false.into())
                        }
                        FunctionKind::UserFunction(func) => {
                            for v in self.0.by_ref() {
                                let res = func(interpreter, vec![v.into()])?;
                                if bool::from(res) {
                                    return Ok(true.into());
                                }
                            }
                            Ok(false.into())
                        }
                    }
                }
                all : "all" {
                    let mut args = args.into_iter().enumerate();
                    let func = typed!(args: Function);
                    match func {
                        FunctionKind::Function(func) => {
                            for v in self.0.by_ref() {
                                interpreter.call(&func, vec![v.into()], None);
                                let res = interpreter
                                    .run()
                                    .map_err(|err| Box::new(err.value))?
                                    .unwrap_or_default();
                                if !bool::from(res) {
                                    return Ok(false.into());
                                }
                            }
                            Ok(true.into())
                        }
                        FunctionKind::UserFunction(func) => {
                            for v in self.0.by_ref() {
                                let res = func(interpreter, vec![v.into()])?;
                                if !bool::from(res) {
                                    return Ok(false.into());
                                }
                            }
                            Ok(true.into())
                        }
                    }
                }
                collect : "collect" {
                    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new($name {
                            array: self.0.clone().map(|v| v.into()).collect::<Vec<$typ>>()
                    })))))
                }
            }
        }
        userobject! {
            $name : $type_name;
            self
            static (self, _i, args) {
                iter : "iter" {
                    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                        $iterator_name(self.array.clone().into_iter()),
                    )))))
                }
                get : "get" {
                    let mut args = args.into_iter().enumerate();
                    let index = typed!(args: Int);
                    let default = args.next().map(|(_, v)| v);
                    Ok(self.array.get(index as usize).map(|v| Value::Int(*v as i64)).unwrap_or(default.unwrap_or_default()))
                }
                len : "len" {
                    Ok(self.array.len().into())
                }
                contains : "contains" {
                    let mut args = args.into_iter().enumerate();
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    Ok(self.array.contains(&value).into())
                }
                position : "position" {
                    let mut args = args.into_iter().enumerate();
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    Ok(self.array
                        .iter()
                        .position(|v| v == &value)
                        .map(|pos| pos.into())
                        .unwrap_or_default())
                }
                join : "join" {
                    let mut args = args.into_iter().enumerate();
                    let sep = typed!(args: String);
                    Ok(Value::String(
                        self.array
                            .iter()
                            .map(|v| v.to_string())
                            .collect::<Vec<String>>()
                            .join(&sep),
                    ))
                }
                copy : "copy" {
                    Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                        Self { array: self.array.clone() },
                    )))))
                }
            }
            mut (self, _i, args) {
                set : "set" {
                    let mut args = args.into_iter().enumerate();
                    let index = typed!(args: Int);
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    if let Some(old_value) = self.array.get_mut(index as usize) {
                        *old_value = value;
                    }
                    Ok(Value::default())
                }
                push : "push" {
                    let mut args = args.into_iter().enumerate();
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    self.array.push(value);
                    Ok(Value::default())
                }
                pop : "pop" {
                    let mut args = args.into_iter().enumerate();
                    let index = typed!(args: Int?);

                    if let Some(index) = index {
                        let index = index.unsigned_abs() as usize;
                        if self.array.get(index).is_some() {
                            Ok(Value::Int(self.array.remove(index) as i64))
                        } else {
                            Ok(Value::default())
                        }
                    } else {
                        Ok(self.array.pop().map(|v| Value::Int(v as i64)).unwrap_or_default())
                    }
                }
                insert : "insert" {
                    let mut args = args.into_iter().enumerate();
                    let index = typed!(args: Int int => int.unsigned_abs() as usize);
                    let value = typed!(args: $luna_typ v => v.cast()?);
                    if index <= self.array.len() {
                        self.array.insert(index, value);
                    }
                    Ok(Value::default())
                }
                swap : "swap" {
                    let mut args = args.into_iter().enumerate();
                    let index1 = typed!(args: Int int => int.unsigned_abs() as usize);
                    let index2 = typed!(args: Int int => int.unsigned_abs() as usize);
                    if self.array.get(index1).is_some() && self.array.get(index2).is_some() {
                        self.array.swap(index1, index2);
                    }
                    Ok(Value::default())
                }
                clear : "clear" {
                    self.array.clear();
                    Ok(Value::default())
                }
            }
        }
        impl $name {
            pub fn _new(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
                let mut args = args.into_iter().enumerate();
                let length = typed!(args: Int? int => int as usize);
                let mut array = typed!(args: Vector? vector => {
                    let mut array = Vec::with_capacity(length.unwrap_or(1));
                    for value in vector.borrow().iter() {
                        array.push(Self::try_cast(value)?);
                    }
                    array
                })
                .unwrap_or_default();
                if let Some(length) = length {
                    while array.len() < length {
                        array.push(Default::default());
                    }
                }
                Ok(Value::UserObject(Rc::new(RefCell::new(Box::new(
                    Self { array },
                )))))
            }
            pub fn try_cast(value: &Value) -> Result<$typ, Box<dyn Error>> {
                match value {
                    Value::$luna_typ(v) => Ok((*v).cast()?),
                    value => Err(Box::new(ArrayError::InvalidElementType(
                        value.dynamic_typ(),
                    ))),
                }
            }
        }
    };
}
#[derive(Debug, Clone)]
pub struct CastError;
impl Display for CastError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "cannot cast value to desired type")
    }
}
impl Error for CastError {}
pub trait CastFrom<T>: Sized {
    fn cast_from(value: T) -> Result<Self, CastError>;
}
pub trait Cast<T>: Sized {
    fn cast(self) -> Result<T, CastError>;
}
impl<T: CastFrom<U>, U> Cast<T> for U {
    fn cast(self) -> Result<T, CastError> {
        T::cast_from(self)
    }
}
impl<T> CastFrom<T> for T {
    fn cast_from(value: T) -> Result<Self, CastError> {
        Ok(value)
    }
}
impl CastFrom<f64> for f32 {
    fn cast_from(value: f64) -> Result<Self, CastError> {
        if value > Self::MAX as f64 || value < Self::MIN as f64 {
            Err(CastError)
        } else {
            Ok(value as Self)
        }
    }
}
impl CastFrom<f32> for f64 {
    fn cast_from(value: f32) -> Result<Self, CastError> {
        Ok(value as Self)
    }
}
impl CastFrom<i64> for u8 {
    fn cast_from(value: i64) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<i64> for u16 {
    fn cast_from(value: i64) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<i64> for u32 {
    fn cast_from(value: i64) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<i64> for u64 {
    fn cast_from(value: i64) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<i64> for u128 {
    fn cast_from(value: i64) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<i64> for i8 {
    fn cast_from(value: i64) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<i64> for i16 {
    fn cast_from(value: i64) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<i64> for i32 {
    fn cast_from(value: i64) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<i64> for i128 {
    fn cast_from(value: i64) -> Result<Self, CastError> {
        Ok(value as Self)
    }
}
impl CastFrom<u8> for i64 {
    fn cast_from(value: u8) -> Result<Self, CastError> {
        Ok(value as Self)
    }
}
impl CastFrom<u16> for i64 {
    fn cast_from(value: u16) -> Result<Self, CastError> {
        Ok(value as Self)
    }
}
impl CastFrom<u32> for i64 {
    fn cast_from(value: u32) -> Result<Self, CastError> {
        Ok(value as Self)
    }
}
impl CastFrom<u64> for i64 {
    fn cast_from(value: u64) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<u128> for i64 {
    fn cast_from(value: u128) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}
impl CastFrom<i8> for i64 {
    fn cast_from(value: i8) -> Result<Self, CastError> {
        Ok(value as Self)
    }
}
impl CastFrom<i16> for i64 {
    fn cast_from(value: i16) -> Result<Self, CastError> {
        Ok(value as Self)
    }
}
impl CastFrom<i32> for i64 {
    fn cast_from(value: i32) -> Result<Self, CastError> {
        Ok(value as Self)
    }
}
impl CastFrom<i128> for i64 {
    fn cast_from(value: i128) -> Result<Self, CastError> {
        value.try_into().map_err(|_| CastError)
    }
}

define_int_array!(ArrayU8 u8 = Int : "array<u8>"; ArrayIteratorU8 : "array-iterator<u8>");
define_int_array!(ArrayU16 u16 = Int : "array<u16>"; ArrayIteratorU16 : "array-iterator<u16>");
define_int_array!(ArrayU32 u32 = Int : "array<u32>"; ArrayIteratorU32 : "array-iterator<u32>");
define_int_array!(ArrayU64 u64 = Int : "array<u64>"; ArrayIteratorU64 : "array-iterator<u64>");
define_int_array!(ArrayU128 u128 = Int : "array<u128>"; ArrayIteratorU128 : "array-iterator<u128>");
define_int_array!(ArrayI8 i8 = Int : "array<i8>"; ArrayIteratorI8 : "array-iterator<i8>");
define_int_array!(ArrayI16 i16 = Int : "array<i16>"; ArrayIteratorI16 : "array-iterator<i16>");
define_int_array!(ArrayI32 i32 = Int : "array<i32>"; ArrayIteratorI32 : "array-iterator<i32>");
define_int_array!(ArrayI64 i64 = Int : "array<i64>"; ArrayIteratorI64 : "array-iterator<i64>");
define_int_array!(ArrayI128 i128 = Int : "array<i128>"; ArrayIteratorI128 : "array-iterator<i128>");
define_array!(ArrayF32 f32 = Float : "array<f32>"; ArrayIteratorF32 : "array-iterator<f32>");
define_array!(ArrayF64 f64 = Float : "array<f64>"; ArrayIteratorF64 : "array-iterator<f64>");
define_array!(ArrayBool bool = Bool : "array<bool>"; ArrayIteratorBool : "array-iterator<bool>");
define_array!(ArrayChar char = Char : "array<char>"; ArrayIteratorChar : "array-iterator<char>");
