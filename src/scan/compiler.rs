use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::lang::code::{ByteCode, Closure, Location, Upvalue};

use super::position::{Located, Position};

#[derive(Debug, Default)]
pub struct Compiler {
    pub(crate) frames: Vec<CompilerFrame>,
}
#[derive(Debug, Default)]
pub struct CompilerFrame {
    pub(crate) closure: Rc<RefCell<Closure>>,
    pub(crate) scopes: Vec<Scope>,
    pub(crate) registers: usize,
}
#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub(crate) locals: HashMap<String, usize>,
    pub(crate) register_offset: usize,
}

impl Compiler {
    pub fn push_frame(&mut self, frame: CompilerFrame) {
        // if let Some(parent_frame) = self.frame_mut() {
        //     parent_frame
        //         .closure
        //         .borrow_mut()
        //         .children
        //         .push(Rc::clone(&frame.closure));
        //     frame.closure.borrow_mut().parent = Some(Rc::clone(&frame.closure));
        // }
        self.frames.push(frame);
    }
    pub fn pop_frame(&mut self) -> Option<CompilerFrame> {
        self.frames.pop()
    }
    pub fn frame(&self) -> Option<&CompilerFrame> {
        self.frames.last()
    }
    pub fn frame_mut(&mut self) -> Option<&mut CompilerFrame> {
        self.frames.last_mut()
    }
    pub fn get_variable_location(&mut self, ident: &str) -> Option<Location> {
        let frame = self.frame_mut().expect("no frame");
        if let Some(register) = frame.get_local(ident) {
            return Some(Location::Register(register));
        }
        for (depth, frame) in self.frames.iter_mut().rev().skip(1).enumerate() {
            if let Some(register) = frame.get_local(ident) {
                let addr = frame.closure.borrow().upvalues.len();
                frame.closure.borrow_mut().upvalues.push(Upvalue {
                    register,
                    in_stack: depth > 0,
                });
                return Some(Location::Upvalue(addr));
            }
        }
        None
    }
}
impl CompilerFrame {
    pub fn write(&mut self, bytecode: ByteCode, pos: Position) -> usize {
        let addr = self.closure.borrow().code.len();
        self.closure
            .borrow_mut()
            .code
            .push(Located::new(bytecode, pos));
        addr
    }
    pub fn overwrite(&mut self, addr: usize, bytecode: ByteCode, pos: Position) {
        let mut closure = self.closure.borrow_mut();
        let old = closure.code.get_mut(addr).expect("invalid addr");
        *old = Located::new(bytecode, pos);
    }
    pub fn new_register(&mut self) -> usize {
        let addr = self.registers;
        self.registers += 1;
        if self.registers > self.closure.borrow().registers {
            self.closure.borrow_mut().registers = self.registers;
        }
        addr
    }
    pub fn push_scope(&mut self) {
        let register_offset = self.registers;
        self.scopes.push(Scope {
            register_offset,
            ..Default::default()
        });
    }
    pub fn pop_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            self.registers = scope.register_offset;
        }
    }
    pub fn scope(&self) -> Option<&Scope> {
        self.scopes.last()
    }
    pub fn scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }
    pub fn new_local(&mut self, ident: String) -> Option<usize> {
        let register = self.new_register();
        let scope = self.scope_mut().expect("no scope");
        scope.set_local(ident, register)
    }
    pub fn get_local(&self, ident: &str) -> Option<usize> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.get_local(ident).is_some())
            .and_then(|scope| scope.get_local(ident))
    }
}
impl Scope {
    pub fn set_local(&mut self, ident: String, register: usize) -> Option<usize> {
        self.locals.insert(ident, register)
    }
    pub fn get_local(&self, ident: &str) -> Option<usize> {
        self.locals.get(ident).cloned()
    }
}
