extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate rustc_serialize;

extern crate reduced_c_syntax as syntax;

use std::io::{self, Read, Write};

pub mod instr;
pub mod trans;
pub mod opt;

#[derive(Debug)]
pub enum CompileError {
    Syntax(syntax::Error),
    Trans(String)
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &CompileError::Syntax(ref e) => write!(f, "{}", e),
            &CompileError::Trans(ref s) => write!(f, "Translation error: {}", s)
        }
    }
}

impl std::error::Error for CompileError {
    fn description(&self) -> &str {
        match self {
            &CompileError::Syntax(ref e) => e.description(),
            &CompileError::Trans(ref s) => s
        }
    }
}

impl From<syntax::Error> for CompileError {
    fn from(e: syntax::Error) -> CompileError {
        CompileError::Syntax(e)
    }
}

impl From<trans::Error> for CompileError {
    fn from(e: trans::Error) -> CompileError {
        CompileError::Trans(e.0)
    }
}

pub fn compile<R: Read>(mut src: R) -> Result<Vec<(instr::Label, instr::Instruction)>, CompileError> {
    let ast = try!(syntax::parse(&mut src));

    let mut assembly = try!(trans::compile(ast));
    opt::optimize(&mut assembly);
    Ok(assembly)
}

pub fn print_assembly<W: Write>(program: &[(instr::Label, instr::Instruction)], mut dest: W) -> io::Result<()> {
    for &(ref label, ref instruction) in program {
        if let &instr::Label::Name(_) = label {
            try!(write!(dest, "{}:\n", label));
        }
        match instruction {
            &instr::Instruction::Nop => { /* nops are only ever generated as filler */ }
            i => try!(write!(dest, "    {}\n", i))
        }
    }
    Ok(())
}
