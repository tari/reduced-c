use std::borrow::Cow;
use std::iter::Peekable;
use std::marker::PhantomData;

use parser_combinators::{Parser, ParserExt, ParseError};
use parser_combinators::primitives::Error as PError;
use parser_combinators::primitives::{State, Stream, ParseResult, Consumed};
use parser_combinators::{sep_by, many, between, parser, optional, try, choice};

pub type Token = String;

#[derive(Clone)]
pub struct TokenStream<I> where I: Iterator<Item=char> + Clone {
    iter: Peekable<I>,
}

impl<I> TokenStream<I> where I: Iterator<Item=char> + Clone {
    fn new(iter: I) -> TokenStream<I> {
        TokenStream {
            iter: iter.peekable()
        }
    }
}

trait StateExt<I> {
    fn uncons_token(self) -> ParseResult<Token, I>;
}

impl<I> StateExt<I> for State<I> where I: Stream<Item=Token> {
    /// Get a token, skipping whitespace.
    fn uncons_token(self) -> ParseResult<Token, I> {
        let mut state = self;
        loop {
            let (tok, consumed) = try!(state.uncons(|pos, tok| {
                for c in tok.chars() {
                    pos.column += 1;
                    if c == '\n' {
                        pos.column = 1;
                        pos.line += 1;
                    }
                }
            }));

            debug!("uncons_token {:?}", tok);
            state = match consumed {
                Consumed::Empty(s) => return Err(
                    Consumed::Empty(ParseError::new(s.position, PError::Message("End of input".into())))
                ),
                Consumed::Consumed(s) => s
            };

            // Tokens must be only whitespace or not contain any
            assert!(tok.chars().all(<char>::is_whitespace)
                 || tok.chars().all(|c| !c.is_whitespace()));
            // Yield non-whitespace tokens
            if !tok.contains(<char>::is_whitespace) {
                return Ok((tok, Consumed::Consumed(state)));
            }
        }

    }
}


impl<I> Stream for TokenStream<I> where I: Iterator<Item=char> + Clone {
    type Item = Token;

    fn uncons(mut self) -> Result<(Token, TokenStream<I>), ()> {
        #[derive(Debug)]
        enum State {
            Null,
            Whitespace,
            Word,
            Condition
        }

        let mut s = String::new();
        let mut state = State::Null;
        let mut continues = true;
        while continues {
            if let Some(c) = self.iter.peek() {
                let (c, s) = match (state, c) {
                    // Coalesce identifier-like things
                    (State::Null, c) |
                    (State::Word, c) if c.is_alphanumeric() || *c == '_' => (true, State::Word),
                    (State::Word, _) => break,

                    // Coalesce whitespace
                    (State::Null, c) |
                    (State::Whitespace, c) if c.is_whitespace() => (true, State::Whitespace),
                    (State::Whitespace, _) => break,

                    // Conditional operations are all one character or end with '='
                    (State::Null, c) if ['>', '<', '!', '='].contains(c) => (true, State::Condition),
                    (State::Condition, &'=') => (false, State::Condition),
                    (State::Condition, _) => break,

                    // Everything else is consumed singly
                    (State::Null, _) => (false, State::Null)
                };
                continues = c;
                state = s;
            } else {
                break;
            }

            s.push(self.iter.next().unwrap());
        }

        if s.len() == 0 {
            debug!("TokenStream::uncons: end of input");
            Err(())
        } else {
            Ok((s, self))
        }
    }
}

struct Matches<F, I>(F, PhantomData<I>);
impl<F, I> Parser for Matches<F, I> where F: FnMut(&Token) -> bool, I: Stream<Item=Token> {
    type Input = I;
    type Output = Token;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<Token, I> {
        match input.clone().uncons_token() {
            Err(e) => Err(e),
            Ok((t, s)) => {
                if (self.0)(&t) {
                    Ok((t, s))
                } else {
                    Err(Consumed::Empty(
                        ParseError::new(input.position,
                                        PError::Message(Cow::Borrowed("Predicate not satisfied")))
                    ))
                }
            }
        }
    }
}
/*impl<F, I> ParserExt for Matches<F, I>
    where F: FnMut(&Token) -> bool, I: Stream<Item=Token> { }*/

/// Matches if the predicate returns true, otherwise fails.
fn matches<F, I>(predicate: F) -> Matches<F, I>
        where F: FnMut(&Token) -> bool, I: Stream<Item=Token> {
    Matches(predicate, PhantomData)
}

struct Literal<I>(&'static str, PhantomData<I>);
impl<I> Parser for Literal<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = Token;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<Token, I> {
        let (t, s) = match input.clone().uncons_token() {
            Err(e) => return Err(e),
            Ok(tup) => tup,
        };

        if &t[..] == self.0 {
            Ok((t, s))
        } else {
            Err(Consumed::Empty(
                ParseError::new(input.position,
                                PError::Expected(Cow::Borrowed(self.0))
                )
            ))
        }
    }
}

/// A literal token.
///
/// ```
/// let mut parser = literal("asdf");
/// assert_eq!(parser.parse(TokenStream::from_str("asdf")),
///            Ok((), _));
/// ```
fn literal<I>(val: &'static str) -> Literal<I> where I: Stream<Item=Token> {
    Literal(val, PhantomData)
}

struct Type<I>(PhantomData<I>);
impl<I> Parser for Type<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = super::Type;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Type, I>
            where I: Stream<Item=Token> {
        match input.clone().uncons_token() {
            Err(e) => Err(e),
            Ok((t, s)) => {
                let ty = match &t[..] {
                    "int" => super::Type::Int,
                    "void" => super::Type::Void,
                    _ => return Err(Consumed::Empty(
                            ParseError::new(
                                input.position,
                                PError::Expected(Cow::Borrowed("type name"))
                            )
                        )
                    )
                };
                Ok((ty, s))
            }
        }
    }
}

/// A type name, `void` or `int`.
fn ty<I>() -> Type<I> where I: Stream<Item=Token> {
    Type(PhantomData)
}

struct Identifier<I>(PhantomData<I>);
impl<I> Parser for Identifier<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = Token;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<Token, I>
            where I: Stream<Item=Token> {
        match input.clone().uncons_token() {
            Err(e) => Err(e),
            Ok((t, s)) => {
                if t.chars().all(|c| c.is_alphanumeric() || c == '_')
                   && !t.starts_with(|c: char| c.is_numeric()) {
                    Ok((t, s))
                } else {
                    Err(Consumed::Empty(ParseError::new(
                        input.position,
                        PError::Message(Cow::Borrowed("invalid identifier"))
                    )))
                }
            }
        }
    }
}

/// An identifier, a string of characters matching `[a-zA-Z_][a-zA-Z0-9_]*`.
fn identifier<I>() -> Identifier<I> where I: Stream<Item=Token> {
    Identifier(PhantomData)
}

fn integer_literal<I: Stream<Item=Token>>(input: State<I>) -> ParseResult<super::Expression, I> {
    matches(|tok| tok.chars().all(|c| c.is_numeric()))
        .and_then(|s: Token| s.parse::<i8>())
        .map(super::Expression::Literal)
        .parse_state(input)
}

struct SingleExpr<I>(PhantomData<I>);
impl<I> Parser for SingleExpr<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = super::Expression;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Expression, I> {
        let variable = identifier()
            .map(super::Expression::Variable);

        optional(literal("-"))
            .and(parser(integer_literal)
                .or(variable))
            .map(|(neg, val)| if neg.is_some() {
                super::Expression::Negation(Box::new(val))
            } else {
                val
            })
            .parse_state(input)
    }
}

struct ArithExpr<I>(PhantomData<I>);
impl<I> Parser for ArithExpr<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = super::Expression;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Expression, I> {
        // Stricly left-to-right precedence: the LHS of an operation string
        // is always a single value, and the RHS may be further operations.
        // We do it this way to avoid unbounded recursion.
        let mut arithmetic = SingleExpr::<I>(PhantomData)
            .and(choice([literal("+"), literal("-")]))
            .and(expression())
            .map(|((lhs, op), rhs)| match &op[..] {
                "+" => super::Expression::Addition(Box::new((lhs, rhs))),
                "-" => super::Expression::Subtraction(Box::new((lhs, rhs))),
                _ => unreachable!()
            });

        arithmetic.parse_state(input)
    }
}

struct Expression<I>(PhantomData<I>);
impl<I> Parser for Expression<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = super::Expression;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Expression, I> {
        try(ArithExpr(PhantomData))
            .or(SingleExpr(PhantomData))
            .parse_state(input)
    }
}

/// An expression yielding a value.
fn expression<I>() -> Expression<I> where I: Stream<Item=Token> {
    Expression(PhantomData)
}

#[test]
fn test_bin_arith_expr() {
    assert_eq!(tparse(expression(), "1 + 1"),
        super::Expression::Addition(Box::new(
            (super::Expression::Literal(1),
             super::Expression::Literal(1))
        ))
    );

    assert_eq!(tparse(expression(), "x - y"),
        super::Expression::Subtraction(Box::new(
            (super::Expression::Variable("x".to_string()),
             super::Expression::Variable("y".to_string()))
        ))
    );

    assert_eq!(tparse(expression(), "x - 1 + y"),
        super::Expression::Subtraction(Box::new(
            (super::Expression::Variable("x".to_string()),
             super::Expression::Addition(Box::new(
                (super::Expression::Literal(1),
                 super::Expression::Variable("y".to_string()))
            )))
        ))
    );
}

#[test]
fn test_negation_expr() {
    assert_eq!(tparse(expression(), "-x"),
        super::Expression::Negation(Box::new(
            super::Expression::Variable("x".to_string())
        ))
    );
}

struct BooleanExpr<I>(PhantomData<I>);
impl<I> Parser for BooleanExpr<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = super::BooleanExpr;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::BooleanExpr, I> {
        use super::BooleanExpr::*;

        expression()
            .and(choice([literal("=="), literal("!="),
                         literal(">="), literal("<="),
                         literal(">"), literal("<")]))
            .and(expression())
            .map(|((lhs, op), rhs)| match &op[..] {
                "==" => Equal(lhs, rhs),
                "!=" => NotEqual(lhs, rhs),
                ">=" => LessOrEqual(rhs, lhs),
                "<=" => LessOrEqual(lhs, rhs),
                ">" => Greater(lhs, rhs),
                "<" => Greater(rhs, lhs),
                _ => unreachable!()
            })
            .parse_state(input)
    }
}

fn bool_expr<I>() -> BooleanExpr<I> where I: Stream<Item=Token> {
    BooleanExpr(PhantomData)
}

struct Statement<I>(PhantomData<I>);
struct BlockStatement<I>(PhantomData<I>);
impl<I> Parser for BlockStatement<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = super::Statement;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Statement, I> {
        let block = || between(literal("{"), literal("}"),
            many::<Vec<_>, _>(statement())
        );

        let conditional = literal("if")
            .with(between(literal("("), literal(")"), bool_expr()))
            .and(block())
            .and(optional(literal("else").with(block())))
            .map(|((predicate, tb), eb)| super::Statement::Conditional(
                predicate, tb, eb
            ));

        let mut while_loop = literal("while")
            .with(between(literal("("), literal(")"), bool_expr()))
            .and(block())
            .map(|(predicate, block)| super::Statement::While(predicate, block));

        while_loop.or(conditional).parse_state(input)
    }
}
impl<I> Parser for Statement<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = super::Statement;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Statement, I> {

        // TODO initializers only accept literals
        let declaration = literal("int")
            .with(identifier())
            .skip(literal("="))
            .and(parser(integer_literal))
            .map(|(ident, expr)| super::Statement::Declaration(ident, expr));

        let assignment = identifier()
            .skip(literal("="))
            .and(expression())
            .map(|(ident, expr)| super::Statement::Assignment(ident, expr));

        let ret = literal("return")
            .with(expression())
            .map(super::Statement::Return);

        BlockStatement(PhantomData).or(
            ret.or(declaration).or(assignment)
                .skip(literal(";"))
        ).parse_state(input)
    }
}

fn statement<I>() -> Statement<I> {
    Statement(PhantomData)
}

#[test]
fn test_declaration() {
    assert_eq!(tparse(statement(), "int x = 0;"), super::Statement::Declaration(
        "x".into(), super::Expression::Literal(0)
    ));
    // Initializers must be literal values only.
    assert!(statement().parse(TokenStream::new("int x = y + z;".chars())).is_err());
}

#[test]
fn test_assign_statement() {
    assert_eq!(tparse(statement(), "foo = 1;"), super::Statement::Assignment(
        "foo".to_string(), 
        super::Expression::Literal(1)
    ));
}

#[test]
fn test_return_statement() {
    assert_eq!(tparse(statement(), "return 0;"), super::Statement::Return(
        super::Expression::Literal(0)
    ));
}

#[test]
fn test_conditional_statement() {
    use super::BooleanExpr::*;
    use super::Expression::*;

    assert_eq!(tparse(statement(), "if (1 == 1) { }"), super::Statement::Conditional(
        Equal(Literal(1), Literal(1)),
        vec![],
        None
    ));
    assert_eq!(tparse(statement(), "if (0 != 1) { } else { }"), super::Statement::Conditional(
        NotEqual(Literal(0), Literal(1)),
        vec![],
        Some(vec![])
    ));
}

#[test]
fn test_while_statement() {
    use super::BooleanExpr::*;
    use super::Expression::*;

    assert_eq!(tparse(statement(), "while (i > 0) { }"), super::Statement::While(
        Greater(Variable("i".to_string()), Literal(0)),
        vec![],
    ));
}


fn function<I>(input: State<I>) -> ParseResult<super::Function, I>
        where I: Stream<Item=Token> {
    let param_list = between(literal("("), literal(")"),
        sep_by::<Vec<_>, _, _>(ty().and(identifier()),
            literal(",")
        )
    );
    ty()
        .and(identifier())
        .and(param_list)
        .and(between(literal("{"), literal("}"), many::<Vec<_>, _>(statement())))
        .map(|(((ty, name), params), body)|
            super::Function {
                returns: ty,
                name: name,
                parameters: params,
                body: body
            }
        )
        .parse_state(input)
}

pub fn parse_str(s: &str) -> Result<super::Function, super::Error> {
    let stream = TokenStream::new(s.chars());
    let res = parser(function).parse(stream);

    let (f, _) = try!(res);
    Ok(f)
}

#[cfg(test)]
fn tparse<'a, T, P>(mut parser: P, s: &'a str) -> T
        where P: Parser<Input=TokenStream<::std::str::Chars<'a>>, Output=T> {
    parser.parse(TokenStream::new(s.chars())).unwrap().0
}
