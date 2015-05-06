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
                      e.description().to_string())
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

/*
impl Function {
    fn parse(src: &str) -> Result<Function, Error> {

        let int_type = string("int");
        let void_type = string("void");
        let function_type = int_type.or(void_type).map(Type::decode);

        let parameter = int_type.map(Type::decode)
            .skip(space())
            .and(identifier());

        let parameter_list = sep_by::<Vec<_>, _, _>(parameter,
                                                    spaces()
                                                    .with(string(","))
                                                    .with(spaces()));

        let function_body = many::<Vec<_>, _>(parser(Statement::parse));

        let mut function = function_type
            .skip(space())      // Required
            .and(identifier())
            .skip(spaces())     // Optional
            .and(between(string("("), string(")"), parameter_list))
            .skip(spaces())
            .and(between(string("{"), string("}"), function_body))
            .map(|(((ty, name), params), body)| {
                Function {
                    returns: ty,
                    name: name,
                    parameters: params,
                    body: body
                }
            });

        // TODO nicer to read chunks and glue more onto the tail from parsing
        let (out, tail) = try!(function.parse(src));
        Ok(out)
    }
}
*/

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Declaration(String, Expression),
    Return(Expression),
    Assignment(String, Expression),
    Conditional(Expression, Vec<Statement>, Option<Vec<Statement>>),
    While(Expression, Vec<Statement>),
}

/*
impl Statement {
    fn assignment<I>(input: State<I>) -> ParseResult<Statement, I> where I: Stream<Item=char> {
        identifier()
            .skip(spaces())
            .skip(string("="))
            .skip(spaces())
            .and(expr())
            .map(|(ident, exp)| Statement::Assignment(ident, exp))
            .parse_state(input)
    }

    fn ret<I>(input: State<I>) -> ParseResult<Statement, I> where I: Stream<Item=char> {
        string("return")
            .skip(space())
            .with(expr())
            .map(|exp| Statement::Return(exp))
            .parse_state(input)
    }

    fn declaration<I>(input: State<I>) -> ParseResult<Statement, I> where I: Stream<Item=char> {
        string("int")
            .skip(space())
            .with(identifier())
            .skip(spaces())
            .skip(string("="))
            .skip(spaces())
            .and(expr())
            .map(|(name, value)| Statement::Declaration(name, value))
            .parse_state(input)
    }

    fn parse<I>(input: State<I>) -> ParseResult<Statement, I> where I: Stream<Item=char> {
        let statements = || many::<Vec<_>, _>(parser(Statement::parse));

        fn single_stmt<I>(input: State<I>) -> ParseResult<Statement, I> where I: Stream<Item=char> {
            try(parser(Statement::declaration))
                .or(try(parser(Statement::ret)))
                .or(try(parser(Statement::assignment)))
                .skip(spaces())
                .skip(string(";"))
                .parse_state(input)
        }
        let single_stmt = parser(single_stmt);

        // TODO admit booleans only? May not be feasible.
        let boolean_expr = || expr();

        let conditional = string("if")
            .skip(spaces())
            .with(between(string("("), string(")"), boolean_expr()))
            .skip(spaces())
            .and(between(string("{"), string("}"), statements()))
            .and(optional(spaces()
                          .skip(string("else"))
                          .skip(spaces())
                          .with(between(string("{"), string("}"), statements()))))
            .map(|((predicate, then_branch), else_branch)|
                 Statement::Conditional(predicate, then_branch, else_branch));
        let while_loop = string("while")
            .skip(spaces())
            .with(between(string("("), string(")"), boolean_expr()))
            .and(between(string("{"), string("}"), statements()))
            .map(|(predicate, body)| Statement::While(predicate, body));
        let block_stmt = try(conditional).or(try(while_loop));

        let mut stmt = spaces()
            .with(block_stmt.or(single_stmt))
            .skip(spaces());
        stmt.parse_state(input)
    }
}
*/
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Literal(i8),
    Variable(String),
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

#[test]
fn test_statements() {
    let f = parse_str("void f() {
                           int x = 0;
                           if (1) {
                               x = 2;
                           } else {
                           }
                           while (x) {
                               x = x - 1;
                           }
                       }").unwrap();
    assert_eq!(f.body, vec![

    ]);
}

