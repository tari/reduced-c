use std::borrow::Cow;
use std::iter::Peekable;
use std::marker::PhantomData;

use parser_combinators::{Parser, ParserExt, ParseError};
use parser_combinators::primitives::Error as PError;
use parser_combinators::primitives::{State, Stream, ParseResult, Consumed};
use parser_combinators::{sep_by, many, between, parser};

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
            Word
        }

        let mut s = String::new();
        let mut state = State::Null;
        let mut continues = true;
        while continues {
            if let Some(c) = self.iter.peek() {
                let (c, s) = match (state, c) {
                    (State::Null, c) |
                    (State::Word, c) if c.is_alphanumeric() || *c == '_' => (true, State::Word),
                    (State::Word, _) => break,

                    (State::Null, c) |
                    (State::Whitespace, c) if c.is_whitespace() => (true, State::Whitespace),
                    (State::Whitespace, _) => break,

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

/// Matches if the predicate returns true, otherwise fails.
fn matches<F, I>(predicate: F) -> Matches<F, I>
        where F: FnMut(&Token) -> bool, I: Stream<Item=Token> {
    Matches(predicate, PhantomData)
}

struct Literal<I>(&'static str, PhantomData<I>);
impl<I> Parser for Literal<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = ();

    fn parse_state(&mut self, input: State<I>) -> ParseResult<(), I> {
        let (t, s) = match input.clone().uncons_token() {
            Err(e) => return Err(e),
            Ok(tup) => tup,
        };

        if &t[..] == self.0 {
            Ok(((), s))
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

struct Expression<I>(PhantomData<I>);
impl<I> Parser for Expression<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = super::Expression;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Expression, I> {
        let literal = matches(|tok| tok.chars().all(|c| c.is_numeric()))
            .map(|s| super::Expression::Literal(s.parse::<i8>().unwrap()));
        let variable = identifier()
            .map(super::Expression::Variable);

        literal.or(variable)
            .parse_state(input)
    }
}

/// An expression yielding a value.
fn expression<I>() -> Expression<I>
        where I: Stream<Item=Token> {
    Expression(PhantomData)
}

struct Statement<I>(PhantomData<I>);
impl<I> Parser for Statement<I> where I: Stream<Item=Token> {
    type Input = I;
    type Output = super::Statement;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Statement, I> {
        unimplemented!()
    }
}

fn statement<I>() -> Statement<I> {
    Statement(PhantomData)
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
        .map(|(((ty, name), params), body)| {
            super::Function {
                returns: ty,
                name: name,
                parameters: params,
                body: vec![]
            }
        })
        .parse_state(input)
}

pub fn parse_str(s: &str) -> Result<super::Function, super::Error> {
    let stream = TokenStream::new(s.chars());
    let res = parser(function).parse(stream);

    let (f, _) = try!(res);
    Ok(f)
}
