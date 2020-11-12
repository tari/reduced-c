//! AST correctness validation.
//!
//! The main mechanism for this is walking the AST with `walk_ast`, which
//! recursively examines statements and expressions in order of evaluation.

use super::syntax::*;

use std::cell::RefCell;
use std::collections::hash_map::Entry::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct ValidationError(String, SourcePosition);

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "at line {} column {}: {}\n",
            self.1.line, self.1.column, self.0
        )
    }
}

pub fn validate_ast<'a>(ast: &'a Function) -> Vec<ValidationError> {
    let mut errors: Vec<ValidationError> = vec![];

    // Track declared variables and where they were declared.
    let mut vars_map: HashMap<&'a str, &'a SourcePosition> = HashMap::new();

    // Parameters are declarations too, but are not in the statements
    // processed by walk_ast. Handle them.
    for &(ref name, ref pos) in ast.parameters.iter().map(|&(_, ref name_pos)| name_pos) {
        match vars_map.insert(name.as_str(), pos) {
            Some(orig_pos) => {
                errors.push(ValidationError(format!(
                    "Redeclaration of variable '{}' not permitted (first declared at line {} column {})",
                    name, orig_pos.line, orig_pos.column
                ), *pos));
            }
            None => { /* Did not exist, is fine. */ }
        }
    }

    // Cell is kind of a hack here. Would be better to rework walk_ast (again)
    // to enforce that the statement and expression closures are never
    // concurrent (I guess passing state around rather than capturing it).
    let vars = RefCell::new(vars_map);
    walk_ast(
        &mut errors,
        &ast.body,
        &mut |stmt| {
            match *stmt {
                // Return types must match function declaration. 'return' without an
                // expression is not permitted, so any Return node in a Void function
                // is an error.
                Statement::Return(_, pos) => {
                    if ast.returns == Type::Void {
                        return Err(ValidationError(
                            "Function returning void must not return a value".into(),
                            pos,
                        ));
                    }
                }

                // Variables may not be redeclared within a scope.
                Statement::Declaration(ref name, _, ref pos) => {
                    match vars.borrow_mut().entry(name) {
                    Occupied(entry) => return Err(ValidationError(format!(
                        "Redeclaration of variable '{}' not permitted (first declared at line {} column {})",
                        name, entry.get().line, entry.get().column
                    ), *pos)),
                    Vacant(entry) => {
                        entry.insert(pos);
                    }
                }
                }

                // Assigning to a variable requires that it has been declared.
                Statement::Assignment(ref name, _, pos) => {
                    if !vars.borrow().contains_key(name.as_str()) {
                        return Err(ValidationError(
                            format!("Cannot assign to undeclared variable '{}'", name),
                            pos,
                        ));
                    }
                }
                _ => { /* Okay */ }
            }
            Ok(())
        },
        &mut |expr| {
            match *expr {
                // Variables used in expressions must exist.
                Expression::Variable(ref name, pos) => {
                    if !vars.borrow().contains_key(name.as_str()) {
                        return Err(ValidationError(
                            format!("Cannot read value of undeclared variable '{}'", name),
                            pos,
                        ));
                    }
                }
                _ => { /* Okay */ }
            };
            Ok(())
        },
    );

    errors
}

/// Recursively examine an expression, like `walk_ast`.
fn walk_expr<'a, F>(errs: &mut Vec<ValidationError>, expr: &'a Expression, f: &mut F)
where
    F: FnMut(&'a Expression) -> Result<(), ValidationError>,
{
    // Walk subexpressions
    match *expr {
        Expression::Addition(ref lhs, ref rhs) | Expression::Subtraction(ref lhs, ref rhs) => {
            walk_expr(errs, lhs, f);
            walk_expr(errs, rhs, f);
        }
        Expression::Negation(ref expr) => walk_expr(errs, expr, f),
        _ => { /* Others do not recurse */ }
    }

    // Validate whole expression
    match f(expr) {
        Err(e) => {
            errs.push(e);
        }
        Ok(_) => (),
    }
}

fn walk_boolexpr<'a, F>(errs: &mut Vec<ValidationError>, expr: &'a BooleanExpr, f: &mut F)
where
    F: FnMut(&'a Expression) -> Result<(), ValidationError>,
{
    match *expr {
        BooleanExpr::Greater(ref lhs, ref rhs)
        | BooleanExpr::LessOrEqual(ref lhs, ref rhs)
        | BooleanExpr::Equal(ref lhs, ref rhs)
        | BooleanExpr::NotEqual(ref lhs, ref rhs) => {
            walk_expr(errs, lhs, f);
            walk_expr(errs, rhs, f);
        }
    }
}

/// Traverse the AST, running the given closures over statements and expressions respectively.
fn walk_ast<'a, F, G>(errs: &mut Vec<ValidationError>, stmts: &'a [Statement], f: &mut F, g: &mut G)
where
    F: FnMut(&'a Statement) -> Result<(), ValidationError>,
    G: FnMut(&'a Expression) -> Result<(), ValidationError>,
{
    for stmt in stmts {
        // Perform checking in order of evaluation. That means subexpressions
        // first in most cases, and the whole statement last.

        // Recurse into sub-statements and contained expressions
        // TODO how would this handle 'int x = x;'? I think correctly now. Add a test for that.
        // (Disregard the part where decls currently only permit literals..)
        match *stmt {
            Statement::Declaration(_, ref expr, _)
            | Statement::Return(ref expr, _)
            | Statement::Assignment(_, ref expr, _) => {
                walk_expr(errs, expr, g);
            }

            Statement::Conditional(ref predicate, ref tb, ref eb) => {
                walk_boolexpr(errs, predicate, g);
                walk_ast(errs, tb, f, g);
                if let &Some(ref eb) = eb {
                    walk_ast(errs, eb, f, g);
                }
            }
            Statement::While(ref predicate, ref sub) => {
                walk_boolexpr(errs, predicate, g);
                walk_ast(errs, sub, f, g);
            }
        }

        match f(stmt) {
            Ok(_) => (),
            Err(e) => errs.push(e),
        }
    }
}

#[test]
fn test_walk_statements() {
    let mut errs = vec![];
    walk_ast(
        &mut errs,
        &[Statement::Assignment(
            "x".into(),
            Expression::Literal(0),
            SourcePosition { line: 1, column: 1 },
        )],
        &mut |_| {
            Err(ValidationError(
                "test".into(),
                SourcePosition { line: 1, column: 1 },
            ))
        },
        &mut |_| Ok(()),
    );

    assert_eq!(
        errs,
        vec![ValidationError(
            "test".into(),
            SourcePosition { line: 1, column: 1 }
        )]
    );
}

#[test]
fn duplicate_var_decls_error() {
    let func =
        super::syntax::parse(&mut "void main() { int x = 0; int x = 1; }".as_bytes()).unwrap();
    assert_eq!(validate_ast(&func).len(), 1);
}

#[test]
fn duplicate_param_decl_error() {
    let func =
        super::syntax::parse(&mut "void main(int x) { int x = 0; int y = 1; }".as_bytes()).unwrap();
    assert_eq!(validate_ast(&func).len(), 1);
}

#[test]
fn use_undeclared_var_error() {
    let func = super::syntax::parse(&mut "void main() { x = 0; }".as_bytes()).unwrap();
    assert_eq!(validate_ast(&func).len(), 1);
}
