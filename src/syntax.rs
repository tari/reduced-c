use std::error::Error as StdError;
use std::io;

use parser_combinators;

#[path="parser.rs"]
pub mod parser;

// Note that rustc currently suffers severe slowdown with deeply nested types;
// see github.com/rust-lang/issues/21231. To work around that, some of the
// rules in here (particularly big ones) are written as functions rather than
// simple local bindings, at the cost of being a bit more verbose.


#[derive(Debug)]
pub enum Error {
    /// Line, column, description
    Syntax(i32, i32, String),
    Other(Box<StdError>)
}

impl From<parser_combinators::ParseError> for Error {
    fn from(e: parser_combinators::ParseError) -> Error {
        Error::Syntax(e.position.line, e.position.column,
                      format!("{}", e))
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::Other(Box::new(e))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Void,
    Int
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    returns: Type,
    name: String,
    parameters: Vec<(Type, String)>,
    body: Vec<Statement>
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Declaration(String, Expression),
    Return(Expression),
    Assignment(String, Expression),
    Conditional(BooleanExpr, Vec<Statement>, Option<Vec<Statement>>),
    While(BooleanExpr, Vec<Statement>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Literal(i8),
    Variable(String),
    Addition(Box<(Expression, Expression)>),
    Subtraction(Box<(Expression, Expression)>),
    Negation(Box<Expression>)
}

#[derive(Debug, PartialEq, Eq)]
pub enum BooleanExpr {
    Greater(Expression, Expression),
    LessOrEqual(Expression, Expression),
    Equal(Expression, Expression),
    NotEqual(Expression, Expression)
}

pub fn parse<R: io::Read>(input: &mut R) -> Result<Function, Error> {
    let mut s = String::new();
    try!(input.read_to_string(&mut s));
    parser::parse_str(&s[..])
}

pub fn parse_str(s: &str) -> Result<Function, Error> {
    parser::parse_str(s)
}

#[test]
fn test_empty_fn() {
    let f = parse_str("void f(){}").unwrap();
    assert_eq!(f,
               Function {
                   returns: Type::Void,
                   name: "f".to_string(),
                   parameters: vec![],
                   body: vec![]
               });
}

#[test]
fn test_return_param() {
    let f = parse_str("int f(int x) { return x; }").unwrap();
    assert_eq!(f,
               Function {
                   returns: Type::Int,
                   name: "f".to_string(),
                   parameters: vec![
                       (Type::Int, "x".to_string())
                   ],
                   body: vec![
                       Statement::Return(Expression::Variable("x".to_string()))
                   ]
               });
}

#[test]
fn test_assignment() {
    let f = parse_str("void f() { int x = 123; }").unwrap();
    assert_eq!(f.body, vec![
        Statement::Declaration("x".to_string(), Expression::Literal(123))
    ]);
}

