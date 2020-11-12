//! Abstract syntax tree.
//!
//! Defines syntatic constructs and translation of source code into an AST.
//!
//! There are a few variants of AST members that have associated
//! SourcePosition, used for reporting of translation errors. Position
//! information isn't included in most because valid syntax implies
//! translatable code. There are basically three classes of translation errors
//! we can catch:
//!
//!  * Return statement does not match function's declaration.
//!  * Redeclaring an existing variable.
//!  * Using (reading or writing) undeclared variables.
use std::error::Error as StdError;
use std::fmt::Write;
use std::io;

mod parser;

pub use combine::primitives::SourcePosition;

// Note that rustc currently suffers severe slowdown with deeply nested types;
// see github.com/rust-lang/issues/21231. To work around that, some of the
// rules in here (particularly big ones) are written as functions rather than
// simple local bindings, at the cost of being a bit more verbose.

#[derive(Debug)]
pub enum Error {
    /// Line, column, description
    Syntax(String),
    Other(Box<dyn StdError>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Error::Syntax(ref info) => write!(f, "Syntax error {}", info),
            Error::Other(ref e) => write!(f, "Unspecified parse error: {}", e),
        }
    }
}

impl StdError for Error {
    fn description(&self) -> &str {
        match *self {
            Error::Syntax(..) => "invalid syntax",
            Error::Other(_) => "unspecified error",
        }
    }
}

impl<I> From<combine::ParseError<parser::TokenStream<I>>> for Error
where
    I: Iterator<Item = char> + Clone,
{
    fn from(e: combine::ParseError<parser::TokenStream<I>>) -> Error {
        let mut s = format!(
            "at line {}, column {}. Possible causes:\n",
            e.position.line, e.position.column
        );
        for err in e.errors {
            write!(s, " * {}\n", err).unwrap();
        }
        Error::Syntax(s)
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::Other(Box::new(e))
    }
}

#[derive(Debug, PartialEq, Eq)]
/// The type of an expression.
pub enum Type {
    Void,
    Int,
}

#[derive(Debug, PartialEq, Eq)]
/// Basic unit of code.
pub struct Function {
    /// Return type.
    pub returns: Type,
    /// Function name.
    pub name: String,
    /// List of parameters, with type and name.
    pub parameters: Vec<(Type, (String, SourcePosition))>,
    /// Body of function, zero or more statements.
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
/// A single "line" of code, including block statements.
pub enum Statement {
    /// (Name, Value)
    ///
    /// `int x = 0` becomes `Declaration("x", Literal(0))`.
    Declaration(String, Expression, SourcePosition),
    /// Return the expression's value from the current function.
    Return(Expression, SourcePosition),
    /// Set the named variable to the value of the expression.
    Assignment(String, Expression, SourcePosition),
    /// Conditionally execute the first block, and the second otherwise.
    Conditional(BooleanExpr, Vec<Statement>, Option<Vec<Statement>>),
    /// Execute statements in sequence as long as the expression is true.
    While(BooleanExpr, Vec<Statement>),
}

#[derive(Debug, PartialEq, Eq)]
/// Expressions that yield integer values.
pub enum Expression {
    /// Literal integer, such as `123`.
    Literal(i8),
    /// The value of a previously-declared variable.
    Variable(String, SourcePosition),
    /// Sum of two expressions.
    Addition(Box<Expression>, Box<Expression>),
    /// Difference of two expressions (first minus second).
    Subtraction(Box<Expression>, Box<Expression>),
    /// Negate the value of an expression.
    Negation(Box<Expression>),
}

#[derive(Debug, PartialEq, Eq)]
/// Expressions that yield boolean values (for block predicates).
pub enum BooleanExpr {
    /// True if first is greater than second.
    Greater(Expression, Expression),
    /// True if first is less than or equal to second.
    LessOrEqual(Expression, Expression),
    /// True if the values are equal.
    Equal(Expression, Expression),
    /// True if the values are not equal.
    NotEqual(Expression, Expression),
}

/// Parse text into a single `Function`.
pub fn parse<R: io::Read>(input: &mut R) -> Result<Function, Error> {
    let mut s = String::new();
    // TODO Read::chars is unstable but would be great here
    trace!("Reading source to string");
    input.read_to_string(&mut s)?;
    debug!("Read {} characters from source code", s.len());
    let out = parser::parse(s.chars());
    debug!("parsed {:?}", out);
    out
}

pub fn parse_str(s: &str) -> Result<Function, Error> {
    parser::parse(s.chars())
}

#[test]
fn test_empty_fn() {
    let f = parse_str("void f(){}").unwrap();
    assert_eq!(
        f,
        Function {
            returns: Type::Void,
            name: "f".to_string(),
            parameters: vec![],
            body: vec![]
        }
    );
}

#[test]
fn test_return_param() {
    let f = parse_str("int f(int x) { return x; }").unwrap();
    assert_eq!(
        f,
        Function {
            returns: Type::Int,
            name: "f".to_string(),
            parameters: vec![(Type::Int, ("x".to_string(), spos(1, 10)))],
            body: vec![Statement::Return(
                Expression::Variable("x".to_string(), spos(1, 22)),
                spos(1, 15)
            )]
        }
    );
}

#[test]
#[ignore] // Source position is wrong. Why?
fn test_assignment() {
    let f = parse_str("void f() { int x = 123; }").unwrap();
    assert_eq!(
        f.body,
        vec![Statement::Declaration(
            "x".to_string(),
            Expression::Literal(123),
            spos(1, 16)
        )]
    );
}

#[cfg(test)]
fn spos(line: i32, col: i32) -> SourcePosition {
    SourcePosition {
        line: line,
        column: col,
    }
}
