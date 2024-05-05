use super::{
    position::{Located, Position},
    std::FOR_FUNC,
};
use crate::lang::{
    ast::*,
    code::{
        Address, BinaryOperation, ByteCode, Closure, Location, ObjectSize, Register, Source,
        Upvalue, VectorSize,
    },
    value::Value,
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    error::Error,
    rc::Rc,
};

#[derive(Debug, Default)]
pub struct Compiler {
    pub frames: Vec<CompilerFrame>,
    pub path: Option<String>,
}
#[derive(Debug, Default)]
pub struct CompilerFrame {
    pub closure: Rc<RefCell<Closure>>,
    pub(crate) scopes: Vec<Scope>,
    pub(crate) registers: Register,
}
#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub(crate) locals: HashMap<String, Register>,
    pub(crate) register_offset: Register,
    pub(crate) breaks: HashSet<Address>,
    pub(crate) continues: HashSet<Address>,
}
pub trait Compilable {
    type Output;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>>;
}

pub const COMPILER_FRAME_EXPECT: &str = "no compiler frame on stack";
pub const COMPILER_SCOPE_EXPECT: &str = "no scope on stack";
macro_rules! compiler_frame {
    ($compiler:ident) => {
        $compiler.frame().expect(COMPILER_FRAME_EXPECT)
    };
}
macro_rules! compiler_frame_mut {
    ($compiler:ident) => {
        $compiler.frame_mut().expect(COMPILER_FRAME_EXPECT)
    };
}
impl Compiler {
    pub fn new(path: Option<String>) -> Self {
        Self {
            frames: vec![CompilerFrame {
                closure: Rc::new(RefCell::new(Closure {
                    path: path.clone(),
                    ..Default::default()
                })),
                scopes: vec![Scope::default()],
                registers: 0,
            }],
            path,
        }
    }
    pub fn push_frame(&mut self, frame: CompilerFrame) {
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
        if let Some(register) = self.frame_mut().expect("no frame").get_local(ident) {
            return Some(Location::Register(register));
        }
        for (depth, other_frame) in self.frames.iter_mut().rev().skip(1).enumerate() {
            if let Some(register) = other_frame.get_local(ident) {
                let frame = self.frame_mut().expect("no frame");
                let addr = frame.closure.borrow().upvalues.len() as Address;
                let upvalue = Upvalue {
                    register,
                    depth: depth as u8,
                };
                if let Some(addr) = frame
                    .closure
                    .borrow_mut()
                    .upvalues
                    .iter()
                    .position(|upv| upv == &upvalue)
                {
                    return Some(Location::Upvalue(addr as Address));
                }
                frame.closure.borrow_mut().upvalues.push(upvalue);
                return Some(Location::Upvalue(addr));
            }
        }
        None
    }
}
impl CompilerFrame {
    pub fn write(&mut self, bytecode: ByteCode, pos: Position) -> Address {
        let addr = self.closure.borrow().code.len() as Address;
        self.closure
            .borrow_mut()
            .code
            .push(Located::new(bytecode, pos));
        addr
    }
    pub fn overwrite(&mut self, addr: Address, bytecode: ByteCode, pos: Option<Position>) {
        let mut closure = self.closure.borrow_mut();
        let old = closure.code.get_mut(addr as usize).expect("invalid addr");
        *old = Located::new(bytecode, pos.unwrap_or(old.pos.clone()));
    }
    pub fn addr(&self) -> Address {
        self.closure.borrow().code.len() as Address
    }
    pub fn new_const(&mut self, value: Value) -> Address {
        let consts = &mut self.closure.borrow_mut().consts;
        if let Some(pos) = consts.iter().position(|v| v == &value) {
            return pos as Address;
        }
        let addr = consts.len();
        consts.push(value);
        addr as Address
    }
    pub fn new_closure(&mut self, value: Rc<RefCell<Closure>>) -> Address {
        let closures = &mut self.closure.borrow_mut().closures;
        let addr = closures.len();
        closures.push(value);
        addr as Address
    }
    pub fn new_register(&mut self) -> Register {
        let addr = self.registers;
        self.registers += 1;
        if self.registers > self.closure.borrow().registers {
            self.closure.borrow_mut().registers = self.registers;
        }
        addr
    }
    pub fn add_registers(&mut self, amount: Register) {
        self.registers += amount;
        if self.registers > self.closure.borrow().registers {
            self.closure.borrow_mut().registers = self.registers;
        }
    }
    pub fn push_scope(&mut self) {
        let register_offset = self.registers;
        self.scopes.push(Scope {
            register_offset,
            ..Default::default()
        });
    }
    pub fn pop_scope(&mut self) -> Option<Scope> {
        if let Some(scope) = self.scopes.pop() {
            self.registers = scope.register_offset;
            return Some(scope);
        }
        None
    }
    pub fn scope(&self) -> Option<&Scope> {
        self.scopes.last()
    }
    pub fn scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }
    pub fn new_local(&mut self, ident: String) -> Register {
        let register = self.new_register();
        let scope = self.scope_mut().expect("no scope");
        scope.set_local(ident, register);
        register
    }
    pub fn get_local(&self, ident: &str) -> Option<Register> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.get_local(ident).is_some())
            .and_then(|scope| scope.get_local(ident))
    }
}
impl Scope {
    pub fn set_local(&mut self, ident: String, register: Register) -> Option<Register> {
        self.locals.insert(ident, register)
    }
    pub fn get_local(&self, ident: &str) -> Option<Register> {
        self.locals.get(ident).cloned()
    }
}

impl Compilable for Located<Chunk> {
    type Output = Rc<RefCell<Closure>>;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>> {
        let Located { value: chunk, pos } = self;
        compiler.push_frame(CompilerFrame {
            closure: Rc::new(RefCell::new(Closure {
                path: compiler.path.clone(),
                ..Default::default()
            })),
            scopes: vec![Scope::default()],
            registers: 0,
        });
        for stat in chunk.0 {
            stat.compile(compiler)?;
        }
        compiler_frame_mut!(compiler).write(ByteCode::Return { src: None }, pos);
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
        for stat in block.0 {
            stat.compile(compiler)?;
        }
        Ok(None)
    }
}
impl Compilable for Located<Statement> {
    type Output = Option<Source>;
    fn compile(self, compiler: &mut Compiler) -> Result<Self::Output, Located<Box<dyn Error>>> {
        let Located { value: stat, pos } = self;
        match stat {
            Statement::Block(block) => {
                compiler_frame_mut!(compiler).push_scope();
                Located::new(block, pos).compile(compiler)?;
                let scope = compiler_frame_mut!(compiler)
                    .pop_scope()
                    .expect("no scope on stack");
                if !scope.breaks.is_empty() {
                    if let Some(prev_scope) = compiler_frame_mut!(compiler).scope_mut() {
                        prev_scope.breaks.extend(scope.breaks);
                    }
                }
                if !scope.continues.is_empty() {
                    if let Some(prev_scope) = compiler_frame_mut!(compiler).scope_mut() {
                        prev_scope.continues.extend(scope.continues);
                    }
                }
                Ok(None)
            }
            Statement::LetBinding { params, mut exprs } => {
                for Located { value: param, pos } in params.into_iter() {
                    let src = if exprs.is_empty() {
                        Source::default()
                    } else {
                        exprs.remove(0).compile(compiler)?
                    };
                    match param {
                        Parameter::Ident(ident) => {
                            let dst =
                                Location::Register(compiler_frame_mut!(compiler).new_local(ident));
                            compiler_frame_mut!(compiler).write(ByteCode::Move { dst, src }, pos);
                        }
                        Parameter::Object(fields) => {
                            for Located { value: ident, pos } in fields.into_iter() {
                                let field = Source::Constant(
                                    compiler_frame_mut!(compiler)
                                        .new_const(Value::String(ident.clone())),
                                );
                                let dst = Location::Register(
                                    compiler_frame_mut!(compiler).new_local(ident),
                                );
                                compiler_frame_mut!(compiler).write(
                                    ByteCode::Field {
                                        dst,
                                        head: src,
                                        field,
                                    },
                                    pos,
                                );
                            }
                        }
                        Parameter::Vector(idents) => {
                            for (idx, Located { value: ident, pos }) in
                                idents.into_iter().enumerate()
                            {
                                let field = Source::Constant(
                                    compiler_frame_mut!(compiler)
                                        .new_const(Value::Int(idx as isize as i64)),
                                );
                                let dst = Location::Register(
                                    compiler_frame_mut!(compiler).new_local(ident),
                                );
                                compiler_frame_mut!(compiler).write(
                                    ByteCode::Field {
                                        dst,
                                        head: src,
                                        field,
                                    },
                                    pos,
                                );
                            }
                        }
                    }
                }
                Ok(None)
            }
            Statement::Assign { paths, mut exprs } => {
                for Located {
                    value: path,
                    pos: path_pos,
                } in paths.into_iter()
                {
                    let src = if exprs.is_empty() {
                        Source::default()
                    } else {
                        exprs.remove(0).compile(compiler)?
                    };
                    match path {
                        Path::Ident(ident) => {
                            let dst = compiler.get_variable_location(&ident).unwrap_or_else(|| {
                                let addr =
                                    compiler_frame_mut!(compiler).new_const(Value::String(ident));
                                Location::Global(addr)
                            });
                            compiler_frame_mut!(compiler)
                                .write(ByteCode::Move { dst, src }, path_pos);
                        }
                        Path::Field {
                            head,
                            field:
                                Located {
                                    value: field,
                                    pos: _,
                                },
                        } => {
                            let head = head.compile(compiler)?;
                            let field =
                                compiler_frame_mut!(compiler).new_const(Value::String(field));
                            compiler_frame_mut!(compiler).write(
                                ByteCode::SetField {
                                    head: head.into(),
                                    field: Source::Constant(field),
                                    src,
                                },
                                path_pos,
                            );
                        }
                        Path::Index { head, index } => {
                            let head = head.compile(compiler)?;
                            let field = index.compile(compiler)?;
                            compiler_frame_mut!(compiler).write(
                                ByteCode::SetField {
                                    head: head.into(),
                                    field,
                                    src,
                                },
                                path_pos,
                            );
                        }
                    }
                }
                Ok(None)
            }
            Statement::AssignOperation {
                op,
                path:
                    Located {
                        value: path,
                        pos: path_pos,
                    },
                expr,
            } => {
                let src = expr.compile(compiler)?;
                match path {
                    Path::Ident(ident) => {
                        let dst = compiler.get_variable_location(&ident).unwrap_or_else(|| {
                            let addr =
                                compiler_frame_mut!(compiler).new_const(Value::String(ident));
                            Location::Global(addr)
                        });
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Binary {
                                op: op.into(),
                                dst,
                                left: dst.into(),
                                right: src,
                            },
                            path_pos,
                        );
                    }
                    Path::Field {
                        head,
                        field:
                            Located {
                                value: field,
                                pos: _,
                            },
                    } => {
                        let head = head.compile(compiler)?;
                        let field = compiler_frame_mut!(compiler).new_const(Value::String(field));
                        let dst = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Field {
                                dst: Location::Register(dst),
                                head: head.into(),
                                field: Source::Constant(field),
                            },
                            path_pos.clone(),
                        );
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Binary {
                                op: op.into(),
                                dst: Location::Register(dst),
                                left: Source::Register(dst),
                                right: src,
                            },
                            path_pos.clone(),
                        );
                        compiler_frame_mut!(compiler).write(
                            ByteCode::SetField {
                                head: head.into(),
                                field: Source::Constant(field),
                                src: Source::Register(dst),
                            },
                            path_pos,
                        );
                    }
                    Path::Index { head, index } => {
                        let head = head.compile(compiler)?;
                        let field = index.compile(compiler)?;
                        let dst = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Field {
                                dst: Location::Register(dst),
                                head: head.into(),
                                field,
                            },
                            path_pos.clone(),
                        );
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Binary {
                                op: op.into(),
                                dst: Location::Register(dst),
                                left: Source::Register(dst),
                                right: src,
                            },
                            path_pos.clone(),
                        );
                        compiler_frame_mut!(compiler).write(
                            ByteCode::SetField {
                                head: head.into(),
                                field,
                                src: Source::Register(dst),
                            },
                            path_pos,
                        );
                    }
                }
                Ok(None)
            }
            Statement::Call { path, mut args } => {
                let path_location = path.compile(compiler)?;
                let amount = args.len() as u8;
                match amount {
                    0 => {
                        compiler_frame_mut!(compiler).write(
                            ByteCode::CallZero {
                                dst: None,
                                func: path_location.into(),
                            },
                            pos,
                        );
                    }
                    1 => {
                        let arg = args.remove(0).compile(compiler)?;
                        compiler_frame_mut!(compiler).write(
                            ByteCode::CallSingle {
                                dst: None,
                                func: path_location.into(),
                                arg,
                            },
                            pos,
                        );
                    }
                    amount => {
                        let offset = compiler_frame_mut!(compiler).registers;
                        compiler_frame_mut!(compiler).add_registers(amount as Register);
                        for (register, arg) in
                            (offset..offset + amount as Register).zip(args.into_iter())
                        {
                            let pos = arg.pos.clone();
                            let src = arg.compile(compiler)?;
                            compiler_frame_mut!(compiler).write(
                                ByteCode::Move {
                                    dst: Location::Register(register),
                                    src,
                                },
                                pos,
                            );
                        }
                        compiler_frame_mut!(compiler).registers = offset;
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Call {
                                dst: None,
                                func: path_location.into(),
                                offset,
                                amount,
                            },
                            pos,
                        );
                    }
                }
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
                    let dst = compiler_frame_mut!(compiler).new_register();
                    let field_addr = compiler_frame_mut!(compiler).new_const(Value::String(field));
                    compiler_frame_mut!(compiler).write(
                        ByteCode::Field {
                            dst: Location::Register(dst),
                            head,
                            field: Source::Constant(field_addr),
                        },
                        field_pos,
                    );
                    Source::Register(dst)
                };
                let amount = args.len() as u8 + 1;
                match amount {
                    1 => {
                        let offset = compiler_frame_mut!(compiler).registers;
                        let arg = head_location.into();
                        compiler_frame_mut!(compiler).registers = offset;
                        compiler_frame_mut!(compiler).write(
                            ByteCode::CallSingle {
                                dst: None,
                                func,
                                arg,
                            },
                            pos,
                        );
                    }
                    amount => {
                        let offset = compiler_frame_mut!(compiler).registers;
                        let head_register = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Move {
                                dst: Location::Register(head_register),
                                src: head_location.into(),
                            },
                            head_pos,
                        );
                        compiler_frame_mut!(compiler).registers += amount as Register;
                        for (register, arg) in
                            (offset + 1..offset + amount as Register).zip(args.into_iter())
                        {
                            let pos = arg.pos.clone();
                            let src = arg.compile(compiler)?;
                            compiler_frame_mut!(compiler).write(
                                ByteCode::Move {
                                    dst: Location::Register(register),
                                    src,
                                },
                                pos,
                            );
                        }
                        compiler_frame_mut!(compiler).registers = offset;
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Call {
                                dst: None,
                                func,
                                offset,
                                amount,
                            },
                            pos,
                        );
                    }
                }
                Ok(None)
            }
            Statement::Fn {
                path:
                    Located {
                        value: path,
                        pos: path_pos,
                    },
                params,
                var_args: _,
                body,
            } => {
                compiler.push_frame(CompilerFrame {
                    closure: Rc::new(RefCell::new(Closure {
                        path: compiler.path.clone(),
                        ..Default::default()
                    })),
                    scopes: vec![Scope::default()],
                    registers: 0,
                });
                for Located {
                    value: param,
                    pos: _,
                } in params
                {
                    match param {
                        Parameter::Ident(ident) => {
                            compiler_frame_mut!(compiler).new_local(ident);
                        }
                        Parameter::Object(fields) => {
                            let head =
                                Source::Register(compiler_frame_mut!(compiler).new_register());
                            for Located { value: ident, pos } in fields.into_iter() {
                                let field = Source::Constant(
                                    compiler_frame_mut!(compiler)
                                        .new_const(Value::String(ident.clone())),
                                );
                                let dst = Location::Register(
                                    compiler_frame_mut!(compiler).new_local(ident),
                                );
                                compiler_frame_mut!(compiler)
                                    .write(ByteCode::Field { dst, head, field }, pos);
                            }
                        }
                        Parameter::Vector(idents) => {
                            let head =
                                Source::Register(compiler_frame_mut!(compiler).new_register());
                            for (idx, Located { value: ident, pos }) in
                                idents.into_iter().enumerate()
                            {
                                let field = Source::Constant(
                                    compiler_frame_mut!(compiler)
                                        .new_const(Value::Int(idx as isize as i64)),
                                );
                                let dst = Location::Register(
                                    compiler_frame_mut!(compiler).new_local(ident),
                                );
                                compiler_frame_mut!(compiler)
                                    .write(ByteCode::Field { dst, head, field }, pos);
                            }
                        }
                    }
                }
                body.compile(compiler)?;
                compiler_frame_mut!(compiler).write(ByteCode::Return { src: None }, pos.clone());
                let closure = compiler
                    .pop_frame()
                    .expect("no compiler frame on stack")
                    .closure;
                let addr = compiler_frame_mut!(compiler).new_closure(closure);
                match path {
                    Path::Ident(ident) => {
                        let dst = compiler.get_variable_location(&ident).unwrap_or_else(|| {
                            let addr =
                                compiler_frame_mut!(compiler).new_const(Value::String(ident));
                            Location::Global(addr)
                        });
                        compiler_frame_mut!(compiler).write(ByteCode::Function { dst, addr }, pos);
                    }
                    Path::Field {
                        head,
                        field:
                            Located {
                                value: field,
                                pos: _,
                            },
                    } => {
                        let head = head.compile(compiler)?;
                        let field = compiler_frame_mut!(compiler).new_const(Value::String(field));
                        let register = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Function {
                                dst: Location::Register(register),
                                addr,
                            },
                            path_pos.clone(),
                        );
                        compiler_frame_mut!(compiler).write(
                            ByteCode::SetField {
                                head: head.into(),
                                field: Source::Constant(field),
                                src: Source::Register(register),
                            },
                            path_pos,
                        );
                        compiler_frame_mut!(compiler).registers = register;
                    }
                    Path::Index { head, index } => {
                        let head = head.compile(compiler)?;
                        let field = index.compile(compiler)?;
                        let register = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Function {
                                dst: Location::Register(register),
                                addr,
                            },
                            path_pos.clone(),
                        );
                        compiler_frame_mut!(compiler).write(
                            ByteCode::SetField {
                                head: head.into(),
                                field,
                                src: Source::Register(register),
                            },
                            path_pos,
                        );
                        compiler_frame_mut!(compiler).registers = register;
                    }
                }
                Ok(None)
            }
            Statement::LetFn {
                ident:
                    Located {
                        value: ident,
                        pos: _,
                    },
                params,
                var_args: _,
                body,
            } => {
                let dst = Location::Register(compiler_frame_mut!(compiler).new_local(ident));
                compiler.push_frame(CompilerFrame {
                    closure: Rc::new(RefCell::new(Closure::default())),
                    scopes: vec![Scope::default()],
                    registers: 0,
                });
                for Located {
                    value: param,
                    pos: _,
                } in params
                {
                    match param {
                        Parameter::Ident(ident) => {
                            compiler_frame_mut!(compiler).new_local(ident);
                        }
                        Parameter::Object(fields) => {
                            let head =
                                Source::Register(compiler_frame_mut!(compiler).new_register());
                            for Located { value: ident, pos } in fields.into_iter() {
                                let field = Source::Constant(
                                    compiler_frame_mut!(compiler)
                                        .new_const(Value::String(ident.clone())),
                                );
                                let dst = Location::Register(
                                    compiler_frame_mut!(compiler).new_local(ident),
                                );
                                compiler_frame_mut!(compiler)
                                    .write(ByteCode::Field { dst, head, field }, pos);
                            }
                        }
                        Parameter::Vector(idents) => {
                            let head =
                                Source::Register(compiler_frame_mut!(compiler).new_register());
                            for (idx, Located { value: ident, pos }) in
                                idents.into_iter().enumerate()
                            {
                                let field = Source::Constant(
                                    compiler_frame_mut!(compiler)
                                        .new_const(Value::Int(idx as isize as i64)),
                                );
                                let dst = Location::Register(
                                    compiler_frame_mut!(compiler).new_local(ident),
                                );
                                compiler_frame_mut!(compiler)
                                    .write(ByteCode::Field { dst, head, field }, pos);
                            }
                        }
                    }
                }
                body.compile(compiler)?;
                compiler_frame_mut!(compiler).write(ByteCode::Return { src: None }, pos.clone());
                let closure = compiler
                    .pop_frame()
                    .expect("no compiler frame on stack")
                    .closure;
                let addr = compiler_frame_mut!(compiler).new_closure(closure);
                compiler_frame_mut!(compiler).write(ByteCode::Function { dst, addr }, pos);
                Ok(None)
            }
            Statement::If {
                cond,
                case,
                else_case,
            } => {
                let cond = cond.compile(compiler)?;
                let check_addr =
                    compiler_frame_mut!(compiler).write(ByteCode::default(), Position::default());

                compiler_frame_mut!(compiler).push_scope();
                case.compile(compiler)?;
                let scope = compiler_frame_mut!(compiler)
                    .pop_scope()
                    .expect(COMPILER_SCOPE_EXPECT);
                if !scope.breaks.is_empty() {
                    if let Some(prev_scope) = compiler_frame_mut!(compiler).scope_mut() {
                        prev_scope.breaks.extend(scope.breaks);
                    }
                }
                if !scope.continues.is_empty() {
                    if let Some(prev_scope) = compiler_frame_mut!(compiler).scope_mut() {
                        prev_scope.continues.extend(scope.continues);
                    }
                }
                if let Some(else_case) = else_case {
                    let else_addr = compiler_frame_mut!(compiler)
                        .write(ByteCode::default(), Position::default());
                    compiler_frame_mut!(compiler).push_scope();
                    else_case.compile(compiler)?;
                    let scope = compiler_frame_mut!(compiler)
                        .pop_scope()
                        .expect(COMPILER_SCOPE_EXPECT);
                    if !scope.breaks.is_empty() {
                        if let Some(prev_scope) = compiler_frame_mut!(compiler).scope_mut() {
                            prev_scope.breaks.extend(scope.breaks);
                        }
                    }
                    if !scope.continues.is_empty() {
                        if let Some(prev_scope) = compiler_frame_mut!(compiler).scope_mut() {
                            prev_scope.continues.extend(scope.continues);
                        }
                    }
                    let exit_addr = compiler_frame_mut!(compiler).addr();
                    compiler_frame_mut!(compiler).overwrite(
                        check_addr,
                        ByteCode::JumpIf {
                            negative: true,
                            cond,
                            addr: else_addr + 1,
                        },
                        Some(pos.clone()),
                    );
                    compiler_frame_mut!(compiler).overwrite(
                        else_addr,
                        ByteCode::Jump { addr: exit_addr },
                        Some(pos),
                    );
                } else {
                    let exit_addr = compiler_frame_mut!(compiler).addr();
                    compiler_frame_mut!(compiler).overwrite(
                        check_addr,
                        ByteCode::JumpIf {
                            negative: true,
                            cond,
                            addr: exit_addr,
                        },
                        Some(pos.clone()),
                    );
                }

                Ok(None)
            }
            Statement::Match { expr, cases } => {
                let expr = expr.compile(compiler)?;
                let registers = compiler_frame!(compiler).registers;
                let dst = Location::Register(compiler_frame_mut!(compiler).new_register());
                let mut exits = vec![];
                for (pattern, body) in cases {
                    compiler_frame_mut!(compiler).push_scope();
                    pattern.compile_match(compiler, expr, dst)?;
                    let check_addr = compiler_frame_mut!(compiler)
                        .write(ByteCode::default(), Position::default());
                    body.compile(compiler)?;
                    let scope = compiler_frame_mut!(compiler)
                        .pop_scope()
                        .expect(COMPILER_SCOPE_EXPECT);
                    if !scope.breaks.is_empty() {
                        if let Some(prev_scope) = compiler_frame_mut!(compiler).scope_mut() {
                            prev_scope.breaks.extend(scope.breaks);
                        }
                    }
                    if !scope.continues.is_empty() {
                        if let Some(prev_scope) = compiler_frame_mut!(compiler).scope_mut() {
                            prev_scope.continues.extend(scope.continues);
                        }
                    }
                    let exit_addr = compiler_frame_mut!(compiler)
                        .write(ByteCode::default(), Position::default());
                    compiler_frame_mut!(compiler).overwrite(
                        check_addr,
                        ByteCode::JumpIf {
                            negative: true,
                            cond: dst.into(),
                            addr: exit_addr + 1,
                        },
                        Some(pos.clone()),
                    );
                    exits.push(exit_addr);
                }
                let exit_addr = compiler_frame_mut!(compiler).addr();
                for addr in exits {
                    if addr + 1 != exit_addr {
                        compiler_frame_mut!(compiler).overwrite(
                            addr,
                            ByteCode::Jump { addr: exit_addr },
                            Some(pos.clone()),
                        );
                    }
                }
                compiler_frame_mut!(compiler).registers = registers;
                Ok(None)
            }
            Statement::While { cond, body } => {
                let start_addr = compiler_frame_mut!(compiler).addr();
                let cond = cond.compile(compiler)?;
                let check_addr =
                    compiler_frame_mut!(compiler).write(ByteCode::default(), Position::default());
                compiler_frame_mut!(compiler).push_scope();
                body.compile(compiler)?;
                let scope = compiler_frame_mut!(compiler)
                    .pop_scope()
                    .expect(COMPILER_SCOPE_EXPECT);
                let exit_addr = compiler_frame_mut!(compiler)
                    .write(ByteCode::Jump { addr: start_addr }, pos.clone());
                for addr in scope.breaks {
                    compiler_frame_mut!(compiler).overwrite(
                        addr,
                        ByteCode::Jump {
                            addr: exit_addr + 1,
                        },
                        None,
                    );
                }
                for addr in scope.continues {
                    compiler_frame_mut!(compiler).overwrite(
                        addr,
                        ByteCode::Jump { addr: start_addr },
                        None,
                    );
                }
                compiler_frame_mut!(compiler).overwrite(
                    check_addr,
                    ByteCode::JumpIf {
                        negative: true,
                        cond,
                        addr: exit_addr + 1,
                    },
                    Some(pos),
                );
                Ok(None)
            }
            Statement::For {
                ident:
                    Located {
                        value: ident,
                        pos: _,
                    },
                iter,
                body,
            } => {
                //          iter !iter = [iter]
                // start:   next !register = @iter
                //          jumpnull @register *exit
                //          [body]
                //          jump *start
                // exit:    ...

                let iter = iter.compile(compiler)?;
                let register = compiler_frame_mut!(compiler).new_local(ident);
                let func = Source::Global(
                    compiler_frame_mut!(compiler).new_const(Value::String(FOR_FUNC.into())),
                );
                let start_addr = compiler_frame_mut!(compiler).write(
                    ByteCode::CallSingle {
                        dst: Some(Location::Register(register)),
                        func,
                        arg: iter,
                    },
                    pos.clone(),
                );
                let check_addr =
                    compiler_frame_mut!(compiler).write(ByteCode::default(), Position::default());
                compiler_frame_mut!(compiler).push_scope();
                body.compile(compiler)?;
                let scope = compiler_frame_mut!(compiler)
                    .pop_scope()
                    .expect(COMPILER_SCOPE_EXPECT);
                let exit_addr = compiler_frame_mut!(compiler)
                    .write(ByteCode::Jump { addr: start_addr }, pos.clone());
                for addr in scope.breaks {
                    compiler_frame_mut!(compiler).overwrite(
                        addr,
                        ByteCode::Jump {
                            addr: exit_addr + 1,
                        },
                        None,
                    );
                }
                for addr in scope.continues {
                    compiler_frame_mut!(compiler).overwrite(
                        addr,
                        ByteCode::Jump { addr: start_addr },
                        None,
                    );
                }
                compiler_frame_mut!(compiler).overwrite(
                    check_addr,
                    ByteCode::JumpNull {
                        cond: Source::Register(register),
                        addr: exit_addr + 1,
                    },
                    Some(pos),
                );
                Ok(None)
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    let src = expr.compile(compiler)?;
                    compiler_frame_mut!(compiler).write(ByteCode::Return { src: Some(src) }, pos);
                    Ok(Some(src))
                } else {
                    compiler_frame_mut!(compiler).write(ByteCode::Return { src: None }, pos);
                    Ok(Some(Source::Null))
                }
            }
            Statement::Break => {
                let addr = compiler_frame_mut!(compiler).write(ByteCode::None, pos);
                compiler_frame_mut!(compiler)
                    .scope_mut()
                    .expect(COMPILER_SCOPE_EXPECT)
                    .breaks
                    .insert(addr);
                Ok(None)
            }
            Statement::Continue => {
                let addr = compiler_frame_mut!(compiler).write(ByteCode::None, pos);
                compiler_frame_mut!(compiler)
                    .scope_mut()
                    .expect(COMPILER_SCOPE_EXPECT)
                    .continues
                    .insert(addr);
                Ok(None)
            }
        }
    }
}
impl Located<Pattern> {
    fn compile_match(
        self,
        compiler: &mut Compiler,
        expr: Source,
        dst: Location,
    ) -> Result<(), Located<Box<dyn Error>>> {
        let Located {
            value: pattern,
            pos,
        } = self;
        match pattern {
            Pattern::Ident(ident) => {
                let ident = compiler_frame_mut!(compiler).new_local(ident);
                compiler_frame_mut!(compiler).write(
                    ByteCode::Move {
                        dst: Location::Register(ident),
                        src: expr,
                    },
                    pos.clone(),
                );
                compiler_frame_mut!(compiler).write(
                    ByteCode::Move {
                        dst,
                        src: Source::Bool(true),
                    },
                    pos,
                );
            }
            Pattern::Atom(atom) => {
                let atom = Located::new(atom, pos.clone()).compile(compiler)?;
                compiler_frame_mut!(compiler).write(
                    ByteCode::Binary {
                        op: BinaryOperation::EQ,
                        dst,
                        left: expr,
                        right: atom,
                    },
                    pos,
                );
            }
            Pattern::Guard { pattern, cond } => {
                pattern.compile_match(compiler, expr, dst)?;
                let cond = cond.compile(compiler)?;
                compiler_frame_mut!(compiler).write(
                    ByteCode::Binary {
                        op: BinaryOperation::And,
                        dst,
                        left: dst.into(),
                        right: cond,
                    },
                    pos,
                );
            }
        }
        Ok(())
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
                let register = compiler_frame_mut!(compiler).new_register();
                let dst = Location::Register(register);
                compiler_frame_mut!(compiler).write(
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
                let register = compiler_frame_mut!(compiler).new_register();
                let dst = Location::Register(register);
                compiler_frame_mut!(compiler).write(
                    ByteCode::Unary {
                        op: op.into(),
                        dst,
                        src: right,
                    },
                    pos,
                );
                Ok(dst.into())
            }
            Expression::Call { head, mut args } => {
                let head = head.compile(compiler)?;
                let amount = args.len() as u8;
                match amount {
                    0 => {
                        let dst = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::CallZero {
                                dst: Some(Location::Register(dst)),
                                func: head.into(),
                            },
                            pos,
                        );
                        Ok(Source::Register(dst))
                    }
                    1 => {
                        let arg = args.remove(0).compile(compiler)?;
                        let dst = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::CallSingle {
                                dst: Some(Location::Register(dst)),
                                func: head.into(),
                                arg,
                            },
                            pos,
                        );
                        Ok(Source::Register(dst))
                    }
                    amount => {
                        let offset = compiler_frame_mut!(compiler).registers;
                        compiler_frame_mut!(compiler).add_registers(amount as Register);
                        for (register, arg) in
                            (offset..offset + amount as Register).zip(args.into_iter())
                        {
                            let pos = arg.pos.clone();
                            let src = arg.compile(compiler)?;
                            compiler_frame_mut!(compiler).write(
                                ByteCode::Move {
                                    dst: Location::Register(register),
                                    src,
                                },
                                pos,
                            );
                        }
                        compiler_frame_mut!(compiler).registers = offset;
                        let dst = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
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
                }
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
                    let dst = compiler_frame_mut!(compiler).new_register();
                    let field_addr = compiler_frame_mut!(compiler).new_const(Value::String(field));
                    compiler_frame_mut!(compiler).write(
                        ByteCode::Field {
                            dst: Location::Register(dst),
                            head,
                            field: Source::Constant(field_addr),
                        },
                        field_pos,
                    );
                    Source::Register(dst)
                };
                let amount = args.len() as u8 + 1;
                match amount {
                    1 => {
                        let offset = compiler_frame_mut!(compiler).registers;
                        let arg = head.into();
                        compiler_frame_mut!(compiler).registers = offset;
                        let dst = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::CallSingle {
                                dst: Some(Location::Register(dst)),
                                func,
                                arg,
                            },
                            pos,
                        );
                        Ok(Source::Register(dst))
                    }
                    amount => {
                        let offset = compiler_frame_mut!(compiler).registers;
                        let head_register = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Move {
                                dst: Location::Register(head_register),
                                src: head,
                            },
                            head_pos,
                        );
                        compiler_frame_mut!(compiler).add_registers(amount as Register);
                        for (register, arg) in
                            (offset + 1..offset + amount as Register).zip(args.into_iter())
                        {
                            let pos = arg.pos.clone();
                            let src = arg.compile(compiler)?;
                            compiler_frame_mut!(compiler).write(
                                ByteCode::Move {
                                    dst: Location::Register(register),
                                    src,
                                },
                                pos,
                            );
                        }
                        compiler_frame_mut!(compiler).registers = offset;
                        let dst = compiler_frame_mut!(compiler).new_register();
                        compiler_frame_mut!(compiler).write(
                            ByteCode::Call {
                                dst: Some(Location::Register(dst)),
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
            Expression::Field {
                head,
                field:
                    Located {
                        value: field,
                        pos: _,
                    },
            } => {
                let head = head.compile(compiler)?;
                let field =
                    Source::Constant(compiler_frame_mut!(compiler).new_const(Value::String(field)));
                let dst = compiler_frame_mut!(compiler).new_register();
                compiler_frame_mut!(compiler).write(
                    ByteCode::Field {
                        dst: Location::Register(dst),
                        head,
                        field,
                    },
                    pos,
                );
                Ok(Source::Register(dst))
            }
            Expression::Index { head, index } => {
                let head = head.compile(compiler)?;
                let field = index.compile(compiler)?;
                let dst = compiler_frame_mut!(compiler).new_register();
                compiler_frame_mut!(compiler).write(
                    ByteCode::Field {
                        dst: Location::Register(dst),
                        head,
                        field,
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
                compiler_frame_mut!(compiler).new_const(Value::Int(v)),
            )),
            Atom::Float(v) => Ok(Source::Constant(
                compiler_frame_mut!(compiler).new_const(Value::Float(v)),
            )),
            Atom::Bool(v) => Ok(Source::Bool(v)),
            Atom::Char(v) => Ok(Source::Char(v)),
            Atom::String(v) => Ok(Source::Constant(
                compiler_frame_mut!(compiler).new_const(Value::String(v)),
            )),
            Atom::Expression(expr) => expr.compile(compiler),
            Atom::Vector(exprs) => {
                let amount = exprs.len() as VectorSize;
                let register = compiler_frame_mut!(compiler).new_register();
                let dst = Location::Register(register);
                let start = compiler_frame_mut!(compiler).registers;
                let registers = (1..=amount)
                    .map(|_| compiler_frame_mut!(compiler).new_register())
                    .collect::<Vec<Register>>();
                for (expr, register) in exprs.into_iter().zip(registers.into_iter()) {
                    let expr_pos = expr.pos.clone();
                    let src = expr.compile(compiler)?;
                    compiler_frame_mut!(compiler).write(
                        ByteCode::Move {
                            dst: Location::Register(register),
                            src,
                        },
                        expr_pos,
                    );
                }
                compiler_frame_mut!(compiler).registers = start;
                compiler_frame_mut!(compiler).write(ByteCode::Vector { dst, start, amount }, pos);
                Ok(dst.into())
            }
            Atom::Object(entries) => {
                let amount = entries.len() as ObjectSize;
                let register = compiler_frame_mut!(compiler).new_register();
                let dst = Location::Register(register);
                let start = compiler_frame_mut!(compiler).registers;
                let registers = (1..amount * 2 + 1)
                    .map(|_| compiler_frame_mut!(compiler).new_register())
                    .collect::<Vec<Register>>();
                for (
                    (
                        Located {
                            value: key,
                            pos: key_pos,
                        },
                        expr,
                    ),
                    register,
                ) in entries.into_iter().zip(registers.into_iter().step_by(2))
                {
                    let addr = compiler_frame_mut!(compiler).new_const(Value::String(key));
                    let expr_pos = expr.pos.clone();
                    let src = expr.compile(compiler)?;
                    compiler_frame_mut!(compiler).write(
                        ByteCode::Move {
                            dst: Location::Register(register),
                            src: Source::Constant(addr),
                        },
                        key_pos,
                    );
                    compiler_frame_mut!(compiler).write(
                        ByteCode::Move {
                            dst: Location::Register(register + 1),
                            src,
                        },
                        expr_pos,
                    );
                }
                compiler_frame_mut!(compiler).registers = start;
                compiler_frame_mut!(compiler).write(ByteCode::Object { dst, start, amount }, pos);
                Ok(dst.into())
            }
            Atom::If {
                cond,
                case,
                else_case,
            } => {
                let cond = cond.compile(compiler)?;
                let register = compiler_frame_mut!(compiler).new_register();
                let check_addr =
                    compiler_frame_mut!(compiler).write(ByteCode::default(), Position::default());

                let case = case.compile(compiler)?;
                compiler_frame_mut!(compiler).write(
                    ByteCode::Move {
                        dst: Location::Register(register),
                        src: case,
                    },
                    pos.clone(),
                );
                let else_addr =
                    compiler_frame_mut!(compiler).write(ByteCode::default(), Position::default());
                let else_case = else_case.compile(compiler)?;
                compiler_frame_mut!(compiler).write(
                    ByteCode::Move {
                        dst: Location::Register(register),
                        src: else_case,
                    },
                    pos.clone(),
                );
                let exit_addr = compiler_frame_mut!(compiler).addr();
                compiler_frame_mut!(compiler).overwrite(
                    check_addr,
                    ByteCode::JumpIf {
                        negative: true,
                        cond,
                        addr: else_addr + 1,
                    },
                    Some(pos.clone()),
                );
                compiler_frame_mut!(compiler).overwrite(
                    else_addr,
                    ByteCode::Jump { addr: exit_addr },
                    Some(pos),
                );
                Ok(Source::Register(register))
            }
            Atom::Fn {
                params,
                var_args: _,
                body,
            } => {
                compiler.push_frame(CompilerFrame {
                    closure: Rc::new(RefCell::new(Closure::default())),
                    scopes: vec![Scope::default()],
                    registers: 0,
                });
                for Located {
                    value: param,
                    pos: _,
                } in params
                {
                    match param {
                        Parameter::Ident(ident) => {
                            compiler_frame_mut!(compiler).new_local(ident);
                        }
                        Parameter::Object(fields) => {
                            let head =
                                Source::Register(compiler_frame_mut!(compiler).new_register());
                            for Located { value: ident, pos } in fields.into_iter() {
                                let field = Source::Constant(
                                    compiler_frame_mut!(compiler)
                                        .new_const(Value::String(ident.clone())),
                                );
                                let dst = Location::Register(
                                    compiler_frame_mut!(compiler).new_local(ident),
                                );
                                compiler_frame_mut!(compiler)
                                    .write(ByteCode::Field { dst, head, field }, pos);
                            }
                        }
                        Parameter::Vector(idents) => {
                            let head =
                                Source::Register(compiler_frame_mut!(compiler).new_register());
                            for (idx, Located { value: ident, pos }) in
                                idents.into_iter().enumerate()
                            {
                                let field = Source::Constant(
                                    compiler_frame_mut!(compiler)
                                        .new_const(Value::Int(idx as isize as i64)),
                                );
                                let dst = Location::Register(
                                    compiler_frame_mut!(compiler).new_local(ident),
                                );
                                compiler_frame_mut!(compiler)
                                    .write(ByteCode::Field { dst, head, field }, pos);
                            }
                        }
                    }
                }
                body.compile(compiler)?;
                compiler_frame_mut!(compiler).write(ByteCode::Return { src: None }, pos.clone());
                let closure = compiler
                    .pop_frame()
                    .expect("no compiler frame on stack")
                    .closure;
                let addr = compiler_frame_mut!(compiler).new_closure(closure);
                let register = compiler_frame_mut!(compiler).new_register();
                compiler_frame_mut!(compiler).write(
                    ByteCode::Function {
                        dst: Location::Register(register),
                        addr,
                    },
                    pos,
                );
                Ok(Source::Register(register))
            }
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
                    let addr = compiler_frame_mut!(compiler).new_const(Value::String(ident));
                    Ok(Location::Global(addr))
                }
            }
            Path::Field {
                head,
                field:
                    Located {
                        value: field,
                        pos: _,
                    },
            } => {
                let head = head.compile(compiler)?;
                let field =
                    Source::Constant(compiler_frame_mut!(compiler).new_const(Value::String(field)));
                let dst = compiler_frame_mut!(compiler).new_register();
                compiler_frame_mut!(compiler).write(
                    ByteCode::Field {
                        dst: Location::Register(dst),
                        head: head.into(),
                        field,
                    },
                    pos,
                );
                Ok(Location::Register(dst))
            }
            Path::Index { head, index } => {
                let head = head.compile(compiler)?;
                let field = index.compile(compiler)?;
                let dst = compiler_frame_mut!(compiler).new_register();
                compiler_frame_mut!(compiler).write(
                    ByteCode::Field {
                        dst: Location::Register(dst),
                        head: head.into(),
                        field,
                    },
                    pos,
                );
                Ok(Location::Register(dst))
            }
        }
    }
}
