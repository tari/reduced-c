use std::error;
use std::io;
use std::str::FromStr;

use parser_combinators::{Parser, ParserExt};
use parser_combinators::{alpha_num, between, choice, from_iter, many1, sep_by, space, spaces, string};

#[derive(Debug)]
pub enum Ast {
    Function(Type, String, Vec<(Type, String)>, Box<Ast>),
    Expression
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
pub enum Error {
    SyntaxError,
    Other(Box<error::Error>)
}

impl<E: error::Error + 'static> From<E> for Error {
    fn from(e: E) -> Error {
        Error::Other(Box::new(e))
    }
}

impl Ast {
    pub fn parse(src: &str) -> Result<Ast, Error> {
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

        let function_body = spaces();

        let mut function = function_type
            .and(identifier)
            .and(spaces()
                 .with(between(string("("), string(")"), parameter_list)))
            .and(spaces()
                 .with(between(string("{"), string("}"), function_body)))
            .map(|(((ty, name), params), body)| {
                Ast::Function(ty, name, params, Box::new(Ast::Expression))
            });

        // TODO nicer to read chunks and glue more onto the tail from parsing
        let (out, tail) = try!(function.parse(src));
        Ok(out)
    }
}
