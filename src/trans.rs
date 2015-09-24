//! Translation of the AST to instruction stream.

use std;
use std::collections::{hash_set, HashSet, hash_map, HashMap};
use std::ops::Range;

use super::instr::{self, Instruction, Register, Label};
use syntax::{self, Statement, Expression, BooleanExpr, Type};

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

enum IterStaticsState<'a> {
    Constants(hash_set::Iter<'a, i8>),
    Locals(hash_map::Iter<'a, String, i8>),
    Anons(Range<usize>)
}

impl<'a> IterStaticsState<'a> {
    fn next(&mut self) -> Option<(String, i8)> {
        match self {
            &mut IterStaticsState::Constants(ref mut i) => i.next().map(|&value| {
                let sign = if value < 0 {
                    "n"
                } else {
                    ""
                };
                (format!("#{}{}", value.abs(), sign), value)
            }),
            &mut IterStaticsState::Locals(ref mut i) => i.next().map(|(name, &value)| {
                (name.clone(), value)
            }),
            &mut IterStaticsState::Anons(ref mut i) => i.next().map(|n| {
                (format!(".t{}", n), 0)
            })
        }
    }
}

struct IterStatics<'a> {
    state: IterStaticsState<'a>,
    ctxt: &'a VariableContext
}

impl<'a> Iterator for IterStatics<'a> {
    type Item = (String, i8);

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.state.next();
        if val.is_some() {
            return val;
        } else {
            let next_state = match self.state {
                IterStaticsState::Constants(_) =>
                    IterStaticsState::Locals(self.ctxt.locals.iter()),
                IterStaticsState::Locals(_) =>
                    IterStaticsState::Anons(0..self.ctxt.anon_count),
                IterStaticsState::Anons(_) => return None
            };
            self.state = next_state;
            self.next()
        }
    }
}


impl VariableContext {
    fn new() -> VariableContext {
        VariableContext {
            constants: HashSet::new(),
            locals: HashMap::new(),
            anon_count: 0,
            jump_count: 0
        }
    }

    fn iter_statics<'a>(&'a self) -> IterStatics<'a> {
        IterStatics {
            state: IterStaticsState::Constants(self.constants.iter()),
            ctxt: &self
        }
    }

    /// Create a new variable with the specified name and initial value.
    fn create(&mut self, name: &str, value: i8) -> Label {
        // TODO what about shadowing? Still need to do *something* on name
        // collision even if the entire program is treated as one context.
        self.locals.insert(name.into(), value);
        Label::Name(name.into())
    }

    /// Get the label for the variable with the given name.
    fn get(&mut self, name: &str) -> Option<Label> {
        if self.locals.contains_key(name) {
            Some(Label::Name(name.into()))
        } else {
            None
        }
    }

    /// Get or create a value from the constant pool.
    fn constant(&mut self, value: i8) -> Label {
        self.constants.insert(value);

        // Generate a label and return it
        let prefix = if value < 0 {
            "n"
        } else {
            ""
        };
        // Cast up to i16 since abs(-128) is not representable as i8.
        Label::Name(format!("#{}{}", prefix, (value as i16).abs()))
    }

    /// Create an anonymous local with initial value 0.
    fn anon(&mut self) -> Label {
        self.anon_count += 1;

        let label = format!(".t{}", self.anon_count);
        self.locals.insert(label.clone(), 0).expect("seqnum collision in temporaries");
        Label::Name(label)
    }

    /// Get the label for a function's return value.
    fn function_return(&mut self) -> Label {
        self.locals.insert(".RES".into(), 0);
        Label::Name(".RES".into())
    }

    fn jump_target(&mut self) -> Label {
        self.jump_count += 1;
        Label::Name(format!("_{}", self.jump_count))
    }
}

/// A list of instructions, each of which may have a label associated.
pub type Program = Vec<(Label, instr::Instruction)>;

pub fn compile(ast: syntax::Function) -> Program {
    let mut context = VariableContext::new();
    let mut program: Program = vec![];

    // Create function parameters.
    for (ty, name) in ast.parameters.into_iter() {
        assert_eq!(ty, syntax::Type::Int);
        context.create(&name, 0);
    }

    // Create the function body
    debug!("Expanding {} statements", ast.body.len());
    for stmt in ast.body.into_iter() {
        expand_statement(stmt, &mut program, &mut context);
    }
    // Code always ends with a halt
    program.push((Label::None, Instruction::Halt));

    info!("Statement expansion complete, generated {} instruction(s)", program.len());
    debug!("Complete program: {:?}", &program);

    // Emit label and initial value for all named storage (which includes anonymous temporaries).
    program.extend(context.iter_statics().map(|(label, x)| {
        (Label::Name(label), Instruction::Value(x))
    }));

    // If function returns non-void, allocate return value.
    if ast.returns != Type::Void {
        program.push((Label::Name(".RES".into()), Instruction::Value(0)));
    }

    program
}

/// Expand an expression into a program fragment, with the expression's value
/// in the given label.
fn expand_expr(expr: syntax::Expression, out: Label,
               ctxt: &mut VariableContext) -> Program {
    unimplemented!()
}

fn expand_statement(stmt: syntax::Statement, program: &mut Vec<(Label, Instruction)>,
                    context: &mut VariableContext) {
    match stmt {
        Statement::Declaration(name, value) => {
            if let Expression::Literal(x) = value {
                context.create(&name, x);
            } else {
                panic!("BUG: expected literal in declaration, got {:?}", value)
            }
        }

        Statement::Return(expr) => {
            let result = context.function_return();
            let fragment = expand_expr(expr, result, context);
            program.extend(fragment.into_iter());
        }

        Statement::Assignment(name, expr) => {
            let var = context.get(&name).expect("Variable used before declaration.");
            let fragment = expand_expr(expr, var, context);
            program.extend(fragment.into_iter());
        }

        Statement::Conditional(predicate, then_block, mut else_block) => {
            // Normalize blocks to Option to simplify swapping
            let mut then_block = Some(then_block);

            // Lower the predicate to a machine instruction with lhs and rhs
            let (comparison, lhs, rhs): (fn(Label) -> Instruction, Expression, Expression)
                     = match predicate {
                BooleanExpr::Greater(l, r) => (Instruction::BranchGreater, l, r),
                // Logical inversion of "greater than" is swapping the operands
                BooleanExpr::LessOrEqual(l, r) => (Instruction::BranchGreater, r, l),

                BooleanExpr::NotEqual(l, r) => (Instruction::BranchNotEqual, l, r),
                // Logical inversion of "branch if not equal" is swapping the
                // `then` and `else` blocks.
                BooleanExpr::Equal(l, r) => {
                    std::mem::swap(&mut then_block, &mut else_block);
                    (Instruction::BranchNotEqual, l, r)
                }
            };

            // Evaluate the predicate
            let lhs_anon = context.anon();
            program.extend(expand_expr(lhs, lhs_anon.clone(), context));
            let rhs_anon = context.anon();
            program.extend(expand_expr(rhs, rhs_anon.clone(), context));

            let then_label = context.jump_target();
            let end_label = context.jump_target();

            // Emit comparison and jump. Note that we emit the else block first
            // because the branches are taken if the comparison succeeds, so
            // we jump on condition success and fall through otherwise.
            program.push((Label::None, Instruction::Load(lhs_anon, Register(0))));
            program.push((Label::None, Instruction::Load(rhs_anon, Register(1))));
            program.push((Label::None, Instruction::Compare(Register(0), Register(1))));
            program.push((Label::None, comparison(then_label.clone())));

            // Emit 'else' block
            if let Some(block) = else_block {
                for stmt in block {
                    expand_statement(stmt, program, context);
                }
            }
            program.push((Label::None, Instruction::Jump(end_label.clone())));

            // Emit 'then' block
            // Set the label on this block. Bit of a hack with NOP insertion, but assume
            // we can do a peephole optimization later to drop any that appear.
            program.push((then_label, Instruction::Nop));
            if let Some(block) = then_block {
                for stmt in block {
                    expand_statement(stmt, program, context);
                }
            }
            
            // Emit the end label
            program.push((end_label, Instruction::Nop));
        }

        Statement::While(predicate, statements) => {
            unimplemented!()
        }
    }
}

#[test]
fn empty_program() {
    assert_eq!(compile(syntax::Function {
            returns: Type::Void,
            name: "main".into(),
            parameters: vec![],
            body: vec![]
        }),
        [(Label::None, Instruction::Halt)]
    );
}
