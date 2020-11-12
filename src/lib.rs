#[macro_use]
extern crate log;

use std::io::{self, Read, Write};

pub mod instr;
pub mod opt;
pub mod syntax;
pub mod trans;
pub mod validate;

#[derive(Debug)]
pub enum CompileError {
    Syntax(syntax::Error),
    Validation(Vec<validate::ValidationError>),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &CompileError::Syntax(ref e) => write!(f, "ERROR {}", e),
            &CompileError::Validation(ref es) => {
                for err in es {
                    write!(f, "ERROR {}", err)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for CompileError {
    fn description(&self) -> &str {
        ""
    }
}

impl From<syntax::Error> for CompileError {
    fn from(e: syntax::Error) -> CompileError {
        CompileError::Syntax(e)
    }
}

impl From<Vec<validate::ValidationError>> for CompileError {
    fn from(e: Vec<validate::ValidationError>) -> CompileError {
        CompileError::Validation(e)
    }
}

pub fn compile<R: Read>(
    mut src: R,
) -> Result<Vec<(instr::Label, instr::Instruction)>, CompileError> {
    debug!("Beginning parse");
    let ast = syntax::parse(&mut src)?;
    debug!("Finished parsing source code");

    debug!("Checking AST validity");
    let verrs = validate::validate_ast(&ast);
    if verrs.len() > 0 {
        return Err(verrs.into());
    }
    debug!("Finished checking AST validity");

    debug!("Beginning compilation");
    let mut assembly = trans::compile(ast);
    debug!("Completed compilation to {} instructions", assembly.len());

    debug!("Beginning optimization");
    opt::optimize(&mut assembly);
    debug!("Completed optimization to {} instructions", assembly.len());

    Ok(assembly)
}

pub fn print_assembly<W: Write>(
    program: &[(instr::Label, instr::Instruction)],
    mut dest: W,
) -> io::Result<()> {
    for &(ref label, ref instruction) in program {
        if let &instr::Label::Name(_) = label {
            write!(dest, "{}:\n", label)?;
        }
        match instruction {
            &instr::Instruction::Nop => { /* nops are only ever generated as filler */ }
            i => write!(dest, "    {}\n", i)?,
        }
    }
    Ok(())
}
