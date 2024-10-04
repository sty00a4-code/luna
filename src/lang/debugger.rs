use super::{
    interpreter::{Interpreter, RunTimeError},
    value::Value,
};
use crate::luna_impl::position::Located;
use ratatui::{
    backend::CrosstermBackend,
    crossterm::{
        event::{self, Event, KeyCode},
        terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
        ExecutableCommand,
    },
    layout::{Constraint, Layout},
    widgets::{Block, Paragraph},
    Frame, Terminal,
};
use std::fmt::Write;
use std::io::stdout;

pub struct Debugger {
    pub interpreter: Interpreter,
}
#[derive(Debug, Clone, PartialEq)]
pub enum DebugEvent {
    Quit,
    Step,
}
impl Debugger {
    pub fn new(interpreter: Interpreter) -> Self {
        Self { interpreter }
    }
    pub fn run(&mut self) -> Result<Option<Value>, Located<RunTimeError>> {
        let level = self.interpreter.call_frames.len();
        enable_raw_mode().expect("cannot enter debugger mode");
        stdout()
            .execute(EnterAlternateScreen)
            .expect("cannot enter debugger mode");
        let mut terminal =
            Terminal::new(CrosstermBackend::new(stdout())).expect("cannot enter debugger mode");
        loop {
            terminal.draw(|frame| self.ui(frame)).expect("cannot draw");
            if let Some(event) = self.handle_events() {
                match event {
                    DebugEvent::Quit => break,
                    DebugEvent::Step => {
                        let value = self.interpreter.step()?;
                        if self.interpreter.call_frames.len() < level
                            || self.interpreter.call_frames.is_empty()
                        {
                            disable_raw_mode().expect("cannot exit debugger mode");
                            stdout()
                                .execute(LeaveAlternateScreen)
                                .expect("cannot exit debugger mode");
                            return Ok(value);
                        }
                    }
                }
            }
        }
        disable_raw_mode().expect("cannot exit debugger mode");
        stdout()
            .execute(LeaveAlternateScreen)
            .expect("cannot exit debugger mode");
        Ok(None)
    }

    fn handle_events(&mut self) -> Option<DebugEvent> {
        if event::poll(std::time::Duration::from_millis(50)).unwrap_or_default() {
            if let Ok(Event::Key(key)) = event::read() {
                if key.kind == event::KeyEventKind::Press {
                    match key.code {
                        KeyCode::Char('q') => return Some(DebugEvent::Quit),
                        KeyCode::Char('n') => return Some(DebugEvent::Step),
                        _ => {}
                    }
                }
            }
        }
        None
    }

    fn ui(&self, frame: &mut Frame) {
        let [code_area, stack_area, call_stack_area] = Layout::vertical([
            Constraint::Percentage(50),
            Constraint::Percentage(35),
            Constraint::Percentage(15),
        ])
        .areas(frame.area());
        let code_block = Paragraph::new({
            let mut f = String::new();
            let call_frame = self.interpreter.call_frames.last().unwrap();
            let function = call_frame.function.borrow();
            let closure = function.closure.borrow();
            for (addr, bytecode) in closure.code.iter().enumerate() {
                if call_frame.idx as usize == addr {
                    writeln!(f, " -> [{addr:04}] {bytecode}").unwrap();
                } else {
                    writeln!(f, "    [{addr:04}] {bytecode}").unwrap();
                }
            }
            writeln!(f, "upvalues:").unwrap();
            for (addr, upvalue) in closure.upvalues.iter().enumerate() {
                writeln!(
                    f,
                    "    [{addr}] register: {}, depth: {}",
                    upvalue.register, upvalue.depth
                )
                .unwrap();
            }
            writeln!(f, "constants:").unwrap();
            for (addr, value) in closure.consts.iter().enumerate() {
                writeln!(f, "    [{addr}] {}: {:?}", value.typ(), value).unwrap();
            }
            writeln!(f, "closures:").unwrap();
            for (addr, closure) in closure.closures.iter().enumerate() {
                writeln!(f, "    [{addr}] {:08x?}", closure.as_ptr()).unwrap();
            }
            writeln!(f).unwrap();
            for closure in closure.closures.iter() {
                write!(f, "{}", closure.borrow()).unwrap();
            }
            f
        })
        .block(Block::bordered().title("Code"));
        let stack_block = Paragraph::new({
            let mut f = String::new();
            let call_frame = self.interpreter.call_frames.last().unwrap();
            for (addr, value) in call_frame.stack.iter().enumerate().rev() {
                writeln!(f, "[{addr:04}] {:?}", value.borrow()).unwrap();
            }
            f
        })
        .block(Block::bordered().title("Stack"));
        let call_stack_block = Paragraph::new({
            let mut f = String::new();
            for (addr, call_frame) in self.interpreter.call_frames.iter().enumerate().rev() {
                writeln!(f, "[{addr:04}] {}", call_frame.path().unwrap_or("<?>".to_string())).unwrap();
            }
            f
        })
        .block(Block::bordered().title("Call Stack"));
        frame.render_widget(code_block, code_area);
        frame.render_widget(stack_block, stack_area);
        frame.render_widget(call_stack_block, call_stack_area);
    }
}
