use std::error;
use std::io;

use parser_combinators::{Parser, ParserExt};
use parser_combinators::primitives::{State, Stream, ParseResult};
use parser_combinators::{alpha_num, between, choice, digit, many, many1,
                         parser, sep_by, space, spaces, string};

#[derive(Debug)]
pub enum Error {
    SyntaxError,
    Other(Box<error::Error>)
}

impl<E: error::Error + 'static> From<E> for Error {
    fn from(e: E) -> Error {
        Error::Other(Box::new(e))
    }
}

#[derive(Debug)]
pub enum Type {
    Void,
    Int
}

impl Type {
    fn decode(s: &str) -> Type {
        match s {
            "int" => Type::Int,
            "void" => Type::Void,
            _ => panic!("No such type: {}", s)
        }
    }
}

#[derive(Debug)]
pub struct Function {
    returns: Type,
    name: String,
    parameters: Vec<(Type, String)>,
    body: Vec<Statement>
}

impl Function {
    fn parse(src: &str) -> Result<Function, Error> {
        let identifier = space()
            .with(many1::<String, _>(alpha_num()));

        let int_type = string("int");
        let void_type = string("void");
        let function_type = choice([int_type.clone(), void_type]).map(Type::decode);

        let parameter = int_type.map(Type::decode)
            .and(identifier.clone());
        let parameter_list = sep_by::<Vec<_>, _, _>(parameter,
                                                    spaces()
                                                    .with(string(","))
                                                    .with(spaces()));

        let function_body = many::<Vec<_>, _>(parser(Statement::parse));

        let mut function = function_type
            .and(identifier)
            .and(spaces()
                 .with(between(string("("), string(")"), parameter_list)))
            .and(spaces()
                 .with(between(string("{"), string("}"), function_body)))
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

#[derive(Debug)]
pub enum Statement {
    Return(Expression)
}

impl Statement {
    fn parse<I>(input: State<I>) -> ParseResult<Statement, I> 
            where I: Stream<Item=char> {
        let expr = parser(Expression::parse);

        let ret = string("return")
            .with(space())
            .with(expr)
            .map(|expr| Statement::Return(expr));

        let mut stmt = ret.skip(string(";"));
        stmt.parse_state(input)
    }
}

#[derive(Debug)]
pub enum Expression {
    Literal(i8)
}

impl Expression {
    fn parse<I>(input: State<I>) -> ParseResult<Expression, I>
            where I: Stream<Item=char> {
        let literal = many1::<String, _>(digit())
            .map(|s| Expression::Literal(s.parse::<i8>().unwrap()));

        let mut expr = literal;
        expr.parse_state(input)
    }
}

pub fn parse<R: io::Read>(input: &mut R) -> Result<Function, Error> {
    let mut s = String::new();
    try!(input.read_to_string(&mut s));
    Function::parse(&s)
}

pub fn parse_str(input: &str) -> Result<Function, Error> {
    parse(&mut input.as_bytes())
}
