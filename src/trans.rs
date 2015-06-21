//! Translation of the AST to instruction stream.

use std;
use std::collections::{HashSet, HashMap};
use std::io::Result as IoResult;
use std::io::Write;

use super::instr::{self, Instruction};
use super::syntax::{self, Statement, Expression};

///
///
/// ## Label names
///
/// Labels are named according to the following rules:
///  * constants: '#[n]<abs>', where 'n' is present for negative values only,
///    and <abs> is the absolute value of the value.
///  * locals: '<name>'
///  * anonymous temporaries: '.t<n>', where <n> is a unique non-negative
///    sequence number.
///  * return values: '.RES' (which is okay because a reduced-C program only
///    ever contains one function).
struct VariableContext {
    /// Constant values
    constants: HashSet<i8>,
    /// All allocations in the current context, a name and initial value.
    locals: HashMap<String, i8>,
    /// Number of anonymous locals allocated globally (used for computing
    /// sequence numbers).
    anon_count: usize,
    /// Number of jump targets emitted (again for sequence numbers)
    jump_count: usize,
}

impl VariableContext {
    fn new() -> VariableContext {
        VariableContext {
            constants: HashSet::new(),
            locals: HashMap::new()
        }
    }

    /// Create a new variable with the specified name and initial value.
    fn create(&mut self, name: &str, value: i8) -> instr::Label {
        // TODO what about shadowing? Still need to do *something* on name
        // collision even if the entire program is treated as one context.
        self.locals.insert(name.into(), value);
        instr::Label::Name(name.into())
    }

    /// Get the label for the variable with the given name.
    fn get(&mut self, name: &str) -> Option<instr::Label> {
        self.locals.get(name)
    }

    /// Get or create a value from the constant pool.
    fn constant(&mut self, value: i8) -> instr::Label {
        self.constants.insert(value);

        // Generate a label and return it
        let prefix = if value < 0 {
            "n"
        } else {
            ""
        };
        // Cast up to i16 since abs(-128) is not representable as i8.
        instr::Label::Name(format!("#{}{}", prefix, (value as i16).abs()))
    }

    /// Create an anonymous local with initial value 0.
    fn anon(&mut self) -> instr::Label {
        self.anon_count += 1;

        let label = format!(".t{}", self.anon_count);
        self.locals.insert(label.clone(), 0).expect("seqnum collision in temporaries");
        instr::Label::Name(label)
    }

    /// Get the label for a function's return value.
    fn function_return(&mut self) -> instr::Label {
        self.locals.insert(".RES".into(), 0);
        instr::Label::Name(".RES".into())
    }

    fn jump_target(&mut self) -> instr::Label {
        self.jump_count += 1;
        instr::Label::Name(format!("_{}", self.jump_count))
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
                let fragment = expand_expr(expr, context.get(&name), &mut context);
                program.extend(fragment.into_iter());
            }

            Statement::Conditional(predicate, then_block, else_block) => {
                // Lower the predicate to a machine instruction with lhs and rhs
                let (comparison, lhs, rhs) = match predicate {
                    BooleanExpr::Greater(l, r) => (Instruction::BranchGreater, l, r),
                    // Logical inversion of "greater than" is swapping the operands
                    BooleanExpr::LessOrEqual(l, r) => (Instruction::BranchGreater, r, l),

                    BooleanExpr::NotEqual(l, r) => (Instruction::BranchNotEqual, l, r),
                    // Logical inversion of "branch if not equal" is swapping the
                    // `then` and `else` blocks.
                    BooleanExpr::Equal(l, r) => {
                        mem::swap(&mut then_block, &mut else_block);
                        (Instruction::BranchNotEqual, l, r)
                    }
                };

                // Evaluate the predicate
                let lhs_anon = context.anon();
                program.extend(expand_expr(lhs, lhs_anon, &mut context));
                let rhs_anon = context.anon();
                program.extend(expand_expr(rhs, rhs_anon, &mut context));

                let then_label = context.jump_target();
                let end_label = context.jump_target();

                // Emit comparison and jump. Note that we emit the else block first
                // because the branches are taken if the comparison succeeds, so
                // we jump on condition success and fall through otherwise.
                program.push((None, Instruction::Load(lhs_anon, Register(0))));
                program.push((None, Instruction::Load(rhs_anon, Register(1))));
                program.push((None, Instruction::Compare(Register(0), Register(1))));
                program.push((None, comparison(then_label)));

                // Emit 'else' block
                program.extend(unimplemented!());
                program.push((None, Instruction::Jump(end_label)));

                // Emit 'then' block
                // Set the label on this block. Bit of a hack with NOP insertion, but assume
                // we can do a peephole optimization later to drop any that appear.
                program.push((then_label, Instruction::Nop));
                program.extend(unimplemented!());
                
                // Emit the end label
                program.push((end_label, Instruction::Nop));
            }

            Statement::While(predicate, statements) => {

            }
        }
    }

    // Code always ends with a halt
    program.push((None, Instruction::Halt));

    // TODO emit variables into the program image
    unimplemented!()
}

/// Expand an expression into a program fragment, with the expression's value
/// in the given label.
fn expand_expr(expr: syntax::Expression, out: instr::Label,
               ctxt: &mut VariableContext) -> Program {
    unimplemented!()
}
