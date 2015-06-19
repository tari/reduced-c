//! Translation of the AST to instruction stream.

use std;
use std::collections::{HashSet, HashMap};
use std::io::Result as IoResult;
use std::io::Write;

use super::instr;
use super::syntax::{self, Statement, Expression};

struct VariableContext {
    /// Constant values
    constants: HashSet<i8>,
    /// All allocations in the current context, a name and initial value.
    locals: HashMap<String, i8>
}

impl VariableContext {
    fn new() -> VariableContext {
        unimplemented!()
    }

    /// Create a new variable with the specified name and initial value.
    fn create(&mut self, name: &str, value: i8) -> instr::Label {
        unimplemented!()
    }

    fn constant(&mut self, value: i8) -> instr::Label {
        self.constants.insert(value);

        // Generate a label and return it
        let prefix = if value < 0 {
            "_"
        } else {
            ""
        };
        // Cast up to i16 since abs(-128) is not representable as i8.
        instr::Label::Name(format!("#{}{}", prefix, (value as i16).abs()))
    }

    /// Create an anonymous local with initial value 0.
    fn anon(&mut self) -> instr::Label {
        unimplemented!()
    }

    /// Get the label for a function's return value.
    fn function_return(&mut self) -> instr::Label {
        self.locals.insert(".RES".into(), 0);
        instr::Label::Name(".RES".into())
    }
}

/// A list of instructions, each of which may have a label associated.
pub type Program = Vec<(Option<String>, instr::Instruction)>;

pub fn compile(ast: syntax::Function) -> Program {
    let mut context = VariableContext::new();
    let mut program: Program = vec![];

    // Create function parameters.
    for (ty, name) in ast.parameters.into_iter() {
        assert_eq!(ty, syntax::Type::Int);
        context.create(&name, 0);
    }

    // Create the function body
    for stmt in ast.body.into_iter() {
        match stmt {
            Statement::Declaration(name, value) => {
                match value {
                    Expression::Literal(x) => {
                        context.create(&name, x);
                    }
                    e => panic!("BUG: expected literal in declaration, got {:?}", e)
                }
            }

            Statement::Return(expr) => {
                let result = context.function_return();
                let fragment = expand_expr(expr, result, &mut context);
                program.extend(fragment.into_iter());
            }

            Statement::Assignment(name, expr) => {
                expand_expr(expr, context.create(&name), &mut context);
            }

            Statement::Conditional(predicate, then_block, else_block) => {

            }

            Statement::While(predicate, statements) => {

            }
        }
    }

    // Code always ends with a halt
    program.push(Instruction::Halt);

    // TODO emit variables into the program image
    unimplemented!()
}

/// Expand an expression into a program fragment, with the expression's value
/// in the given label.
fn expand_expr(expr: syntax::Expression, out: instr::Label,
               ctxt: &mut VariableContext) -> Program {
    unimplemented!()
}
