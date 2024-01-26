#![allow(unused_variables)]
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    error::Error,
    rc::Rc,
};

use crate::lang::{
    ast::*,
    code::{ByteCode, Closure, Location, Source, Upvalue},
    value::Value,
};

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
    pub(crate) breaks: HashSet<usize>,
    pub(crate) continues: HashSet<usize>,
}
pub trait Compilable {
    type Output;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>>;
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            frames: vec![CompilerFrame {
                closure: Rc::new(RefCell::new(Closure::default())),
                scopes: vec![Scope::default()],
                registers: 0,
            }],
        }
    }
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
    pub fn addr(&self) -> usize {
        self.closure.borrow().code.len()
    }
    pub fn new_const(&mut self, value: Value) -> usize {
        let consts = &mut self.closure.borrow_mut().consts;
        let addr = consts.len();
        consts.push(value);
        addr
    }
    pub fn new_closure(&mut self, value: Rc<RefCell<Closure>>) -> usize {
        let closures = &mut self.closure.borrow_mut().closures;
        let addr = closures.len();
        closures.push(value);
        addr
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
    pub fn new_local(&mut self, ident: String) -> usize {
        let register = self.new_register();
        let scope = self.scope_mut().expect("no scope");
        scope.set_local(ident, register);
        register
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

impl Compilable for Located<Chunk> {
    type Output = Rc<RefCell<Closure>>;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>> {
        let Located { value: chunk, pos } = self;
        compiler.push_frame(CompilerFrame {
            closure: Rc::new(RefCell::new(Closure::default())),
            scopes: vec![Scope::default()],
            registers: 0,
        });
        for stat in chunk.0 {
            stat.compile(compiler)?;
        }
        compiler
            .frame_mut()
            .expect("no compiler frame on stack")
            .write(ByteCode::Return { src: None }, pos);
        Ok(compiler
            .pop_frame()
            .expect("no compiler frame on stack")
            .closure)
    }
}
impl Compilable for Located<Block> {
    type Output = Option<Source>;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>> {
        let Located {
            value: block,
            pos: _,
        } = self;
        compiler
            .frame_mut()
            .expect("no compiler frame on stack")
            .push_scope();
        for stat in block.0 {
            stat.compile(compiler)?;
        }
        compiler
            .frame_mut()
            .expect("no compiler frame on stack")
            .pop_scope();
        Ok(None)
    }
}
impl Compilable for Located<Statement> {
    type Output = Option<Source>;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>> {
        let Located { value: stat, pos } = self;
        match stat {
            Statement::Block(block) => Located::new(block, pos).compile(compiler),
            Statement::LetBinding { idents, mut exprs } => {
                for Located { value: ident, pos } in idents.into_iter() {
                    let dst = Location::Register(
                        compiler
                            .frame_mut()
                            .expect("no compiler frame on stack")
                            .new_local(ident),
                    );
                    let src = if exprs.is_empty() {
                        Source::default()
                    } else {
                        exprs.remove(0).compile(compiler)?
                    };
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(ByteCode::Move { dst, src }, pos);
                }
                Ok(None)
            }
            Statement::Assign { paths, mut exprs } => {
                for path in paths.into_iter() {
                    let pos = path.pos.clone();
                    let dst = path.compile(compiler)?;
                    let src = if exprs.is_empty() {
                        Source::default()
                    } else {
                        exprs.remove(0).compile(compiler)?
                    };
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(ByteCode::Move { dst, src }, pos);
                }
                Ok(None)
            }
            Statement::Call { path, args } => {
                let path_location = path.compile(compiler)?;
                let amount = args.len();
                let offset = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .registers;
                let registers = (0..amount)
                    .map(|_| {
                        compiler
                            .frame_mut()
                            .expect("no compiler frame on stack")
                            .new_register()
                    })
                    .collect::<Vec<usize>>();
                for (i, arg) in args.into_iter().enumerate() {
                    let pos = arg.pos.clone();
                    let register = registers[i];
                    let src = arg.compile(compiler)?;
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(
                            ByteCode::Move {
                                dst: Location::Register(register),
                                src,
                            },
                            pos,
                        );
                }
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(
                        ByteCode::Call {
                            dst: None,
                            func: path_location.into(),
                            offset,
                            amount,
                        },
                        pos,
                    );
                Ok(None)
            }
            Statement::SelfCall {
                head,
                field:
                    Located {
                        value: field,
                        pos: field_pos,
                    },
                args,
            } => {
                let head_pos = head.pos.clone();
                let head_location = head.compile(compiler)?;
                let func = {
                    let head: Source = head_location.into();
                    let dst = compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .new_register();
                    let field_addr = compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .new_const(Value::String(field));
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(
                            ByteCode::Field {
                                dst: Location::Register(dst),
                                head,
                                field: Source::Constant(field_addr),
                            },
                            field_pos,
                        );
                    Source::Register(dst)
                };
                let amount = args.len() + 1;
                let offset = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .registers;
                let head_register = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .new_register();
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(
                        ByteCode::Move {
                            dst: Location::Register(head_register),
                            src: head_location.into(),
                        },
                        head_pos,
                    );
                let registers = (1..amount)
                    .map(|_| {
                        compiler
                            .frame_mut()
                            .expect("no compiler frame on stack")
                            .new_register()
                    })
                    .collect::<Vec<usize>>();
                for (i, arg) in args.into_iter().enumerate() {
                    let pos = arg.pos.clone();
                    let register = registers[i];
                    let src = arg.compile(compiler)?;
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(
                            ByteCode::Move {
                                dst: Location::Register(register),
                                src,
                            },
                            pos,
                        );
                }
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(
                        ByteCode::Call {
                            dst: None,
                            func,
                            offset,
                            amount,
                        },
                        pos,
                    );
                Ok(None)
            }
            Statement::Fn {
                path,
                params,
                var_args,
                body,
            } => {
                let dst = path.compile(compiler)?;
                let registers = params.len() + if var_args.is_some() { 1 } else { 0 };
                compiler.push_frame(CompilerFrame {
                    closure: Rc::new(RefCell::new(Closure::default())),
                    scopes: vec![Scope::default()],
                    registers
                });
                for Located { value: param, pos: param_pos } in params {
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .new_local(param);
                }
                body.compile(compiler)?;
                let closure = compiler.pop_frame().expect("no compiler frame on stack").closure;
                let addr = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .new_closure(closure);
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(ByteCode::Function { dst, addr }, pos);
                Ok(None)
            }
            Statement::If {
                cond,
                case,
                else_case,
            } => todo!(),
            Statement::While { cond, body } => todo!(),
            Statement::For { idents, iter, body } => todo!(),
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    let src = expr.compile(compiler)?;
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(ByteCode::Return { src: Some(src) }, pos);
                    Ok(Some(src))
                } else {
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(ByteCode::Return { src: None }, pos);
                    Ok(Some(Source::Null))
                }
            }
            Statement::Break => {
                let addr = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(ByteCode::None, pos);
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .scope_mut()
                    .expect("no scope on stack")
                    .breaks
                    .insert(addr);
                Ok(None)
            }
            Statement::Continue => {
                let addr = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(ByteCode::None, pos);
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .scope_mut()
                    .expect("no scope on stack")
                    .continues
                    .insert(addr);
                Ok(None)
            }
        }
    }
}
impl Compilable for Located<Expression> {
    type Output = Source;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>> {
        let Located { value: expr, pos } = self;
        match expr {
            Expression::Atom(atom) => Located::new(atom, pos).compile(compiler),
            Expression::Binary { op, left, right } => {
                let left = left.compile(compiler)?;
                let right = right.compile(compiler)?;
                let dst = Location::Register(
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .new_register(),
                );
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(
                        ByteCode::Binary {
                            op: op.into(),
                            dst,
                            left,
                            right,
                        },
                        pos,
                    );
                Ok(dst.into())
            }
            Expression::Unary { op, right } => {
                let right = right.compile(compiler)?;
                let dst = Location::Register(
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .new_register(),
                );
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(
                        ByteCode::Unary {
                            op: op.into(),
                            dst,
                            src: right,
                        },
                        pos,
                    );
                Ok(dst.into())
            }
            Expression::Call { head, args } => {
                let head = head.compile(compiler)?;
                let amount = args.len();
                let offset = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .registers;
                let registers = (0..amount)
                    .map(|_| {
                        compiler
                            .frame_mut()
                            .expect("no compiler frame on stack")
                            .new_register()
                    })
                    .collect::<Vec<usize>>();
                for (i, arg) in args.into_iter().enumerate() {
                    let pos = arg.pos.clone();
                    let register = registers[i];
                    let src = arg.compile(compiler)?;
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(
                            ByteCode::Move {
                                dst: Location::Register(register),
                                src,
                            },
                            pos,
                        );
                }
                let dst = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .new_register();
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(
                        ByteCode::Call {
                            dst: Some(Location::Register(dst)),
                            func: head,
                            offset,
                            amount,
                        },
                        pos,
                    );
                Ok(Source::Register(dst))
            }
            Expression::SelfCall {
                head,
                field:
                    Located {
                        value: field,
                        pos: field_pos,
                    },
                args,
            } => {
                let head_pos = head.pos.clone();
                let head = head.compile(compiler)?;
                let func = {
                    let dst = compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .new_register();
                    let field_addr = compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .new_const(Value::String(field));
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(
                            ByteCode::Field {
                                dst: Location::Register(dst),
                                head,
                                field: Source::Constant(field_addr),
                            },
                            field_pos,
                        );
                    Source::Register(dst)
                };
                let amount = args.len() + 1;
                let offset = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .registers;
                let head_register = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .new_register();
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(
                        ByteCode::Move {
                            dst: Location::Register(head_register),
                            src: head,
                        },
                        head_pos,
                    );
                let registers = (1..amount)
                    .map(|_| {
                        compiler
                            .frame_mut()
                            .expect("no compiler frame on stack")
                            .new_register()
                    })
                    .collect::<Vec<usize>>();
                for (i, arg) in args.into_iter().enumerate() {
                    let pos = arg.pos.clone();
                    let register = registers[i];
                    let src = arg.compile(compiler)?;
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .write(
                            ByteCode::Move {
                                dst: Location::Register(register),
                                src,
                            },
                            pos,
                        );
                }
                let dst = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .new_register();
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(
                        ByteCode::Call {
                            dst: None,
                            func,
                            offset,
                            amount,
                        },
                        pos,
                    );
                Ok(Source::Register(dst))
            }
        }
    }
}
impl Compilable for Located<Atom> {
    type Output = Source;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>> {
        let Located { value: atom, pos } = self;
        match atom {
            Atom::Path(path) => Ok(Located::new(path, pos).compile(compiler)?.into()),
            Atom::Null => Ok(Source::Null),
            Atom::Int(v) => Ok(Source::Constant(
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .new_const(Value::Int(v)),
            )),
            Atom::Float(v) => Ok(Source::Constant(
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .new_const(Value::Float(v)),
            )),
            Atom::Bool(v) => Ok(Source::Bool(v)),
            Atom::Char(v) => Ok(Source::Char(v)),
            Atom::String(v) => Ok(Source::Constant(
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .new_const(Value::String(v)),
            )),
            Atom::Expression(expr) => expr.compile(compiler),
            Atom::Vector(_) => todo!(),
            Atom::Object(_) => todo!(),
            Atom::If {
                cond,
                case,
                else_vase,
            } => todo!(),
            Atom::Fn {
                params,
                var_args,
                body,
            } => todo!(),
        }
    }
}
impl Compilable for Located<Path> {
    type Output = Location;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>> {
        let Located { value: atom, pos } = self;
        match atom {
            Path::Ident(ident) => {
                if let Some(location) = compiler.get_variable_location(&ident) {
                    Ok(location)
                } else {
                    let addr = compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .new_const(Value::String(ident));
                    Ok(Location::Global(addr))
                }
            }
            Path::Field {
                head,
                field:
                    Located {
                        value: field,
                        pos: field_pos,
                    },
            } => {
                let head = head.compile(compiler)?;
                let field = Source::Constant(
                    compiler
                        .frame_mut()
                        .expect("no compiler frame on stack")
                        .new_const(Value::String(field)),
                );
                let dst = compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .new_register();
                compiler
                    .frame_mut()
                    .expect("no compiler frame on stack")
                    .write(
                        ByteCode::Field {
                            dst: Location::Register(dst),
                            head: head.into(),
                            field,
                        },
                        pos,
                    );
                Ok(Location::Register(dst))
            }
            Path::Index { head, index } => todo!(),
        }
    }
}
