use std;
use std::fmt;
use std::iter::Peekable;
use std::marker::PhantomData;

use combine;
use combine::primitives::Error as PError;
use combine::primitives::{Consumed, ParseResult, SourcePosition, State, Stream};
use combine::{between, choice, many, optional, parser, r#try as ptry, sep_by};
use combine::{ParseError, Parser, ParserExt};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    data: String,
    //location: SourcePosition
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl std::ops::Deref for Token {
    type Target = String;

    fn deref<'a>(&'a self) -> &'a String {
        &self.data
    }
}

impl combine::primitives::Positioner for Token {
    type Position = SourcePosition;
    fn start() -> SourcePosition {
        SourcePosition { line: 1, column: 1 }
    }

    fn update(&self, position: &mut SourcePosition) {
        debug!("<Token as Positioner>::update({:?}, {:?})", self, position);
        (&self.data).update(position);
    }
}

/** Turn a stream of characters into a stream of tokens. **/
#[derive(Clone)]
pub struct TokenStream<I>
where
    I: Iterator<Item = char> + Clone,
{
    iter: Peekable<I>,
}

impl<I> TokenStream<I>
where
    I: Iterator<Item = char> + Clone,
{
    fn new(iter: I) -> TokenStream<I> {
        TokenStream {
            iter: iter.peekable(),
        }
    }
}

impl<I> Stream for TokenStream<I>
where
    I: Iterator<Item = char> + Clone,
{
    type Item = Token;
    type Range = Token;

    fn uncons(mut self) -> Result<(Token, TokenStream<I>), PError<Token, Token>> {
        #[derive(Debug)]
        enum State {
            Null,
            Whitespace,
            Word,
            Condition,
        }

        let mut s = String::new();
        let mut state = State::Null;
        let mut continues = true;
        while continues {
            if let Some(c) = self.iter.peek() {
                let (c, s) = match (state, c) {
                    // Coalesce identifier-like things
                    (State::Null, c) | (State::Word, c) if c.is_alphanumeric() || *c == '_' => {
                        (true, State::Word)
                    }
                    (State::Word, _) => break,

                    // Coalesce whitespace
                    (State::Null, c) | (State::Whitespace, c) if c.is_whitespace() => {
                        (true, State::Whitespace)
                    }
                    (State::Whitespace, _) => break,

                    // Conditional operations are all one character or end with '='
                    (State::Null, c) if ['>', '<', '!', '='].contains(c) => {
                        (true, State::Condition)
                    }
                    (State::Condition, &'=') => (false, State::Condition),
                    (State::Condition, _) => break,

                    // Everything else is consumed singly
                    (State::Null, _) => (false, State::Null),
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
            Err(PError::end_of_input())
        } else {
            let tok = Token { data: s };
            Ok((tok, self))
        }
    }
}

trait StateExt<I: combine::primitives::Stream> {
    fn uncons_token(self) -> ParseResult<Token, I>;
}

impl<I> StateExt<I> for State<I>
where
    I: Stream<Item = Token>,
{
    /// Get a token, skipping whitespace.
    fn uncons_token(self) -> ParseResult<Token, I> {
        let mut state = self;
        loop {
            let (tok, consumed) = state.uncons()?;

            match consumed {
                Consumed::Empty(_) => {
                    return Err(Consumed::Empty(
                        // XXX bogus position
                        ParseError::end_of_input(SourcePosition { line: 0, column: 0 }),
                    ))
                }
                Consumed::Consumed(s) => {
                    state = s;
                }
            }

            debug!("uncons_token {:?}", tok);

            // Tokens must be only whitespace or not contain any
            assert!(
                tok.chars().all(<char>::is_whitespace) || tok.chars().all(|c| !c.is_whitespace())
            );
            // Yield non-whitespace tokens
            if !tok.contains(<char>::is_whitespace) {
                return Ok((tok, Consumed::Consumed(state)));
            }
        }
    }
}

struct Matches<'a, F, I>(F, Option<&'a str>, PhantomData<I>);
impl<'a, F, I> Parser for Matches<'a, F, I>
where
    F: FnMut(&Token) -> bool,
    I: Stream<Item = Token>,
{
    type Input = I;
    type Output = Token;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<Token, I> {
        match input.clone().uncons_token() {
            Err(e) => Err(e),
            Ok((t, s)) => {
                if (self.0)(&t) {
                    Ok((t, s))
                } else {
                    let message = match &self.1 {
                        &None => "predicate failed",
                        &Some(s) => s,
                    };
                    Err(Consumed::Empty(ParseError::new(
                        input.position,
                        PError::Message(message.to_string().into()),
                    )))
                }
            }
        }
    }
}

/// Matches if the predicate returns true, otherwise fails.
fn matches<'a, F, I>(predicate: F, message: Option<&'a str>) -> Matches<'a, F, I>
where
    F: FnMut(&Token) -> bool,
    I: Stream<Item = Token>,
{
    Matches(predicate, message, PhantomData)
}

struct Literal<I>(&'static str, PhantomData<I>);
impl<I> Parser for Literal<I>
where
    I: Stream<Item = Token>,
{
    type Input = I;
    type Output = String;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<Self::Output, I> {
        let (t, s) = match input.clone().uncons_token() {
            Err(e) => return Err(e),
            Ok(tup) => tup,
        };

        if &t[..] == self.0 {
            Ok((t.data, s))
        } else {
            Err(Consumed::Empty(ParseError::new(
                input.position,
                PError::Expected(self.0.into()),
            )))
        }
    }
}

/// A literal token.
fn literal<I>(val: &'static str) -> Literal<I>
where
    I: Stream<Item = Token>,
{
    Literal(val, PhantomData)
}

struct Type<I>(PhantomData<I>);
impl<I> Parser for Type<I>
where
    I: Stream<Item = Token>,
{
    type Input = I;
    type Output = super::Type;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Type, I>
    where
        I: Stream<Item = Token>,
    {
        match input.clone().uncons_token() {
            Err(e) => Err(e),
            Ok((t, s)) => {
                let ty = match &t[..] {
                    "int" => super::Type::Int,
                    "void" => super::Type::Void,
                    _ => {
                        return Err(Consumed::Empty(ParseError::new(
                            input.position,
                            PError::Expected("type name".into()),
                        )))
                    }
                };
                Ok((ty, s))
            }
        }
    }
}

/// A type name, `void` or `int`.
fn ty<I>() -> Type<I>
where
    I: Stream<Item = Token>,
{
    Type(PhantomData)
}

struct Identifier<I>(PhantomData<I>);
impl<I> Parser for Identifier<I>
where
    I: Stream<Item = Token>,
{
    type Input = I;
    type Output = (String, SourcePosition);

    fn parse_state(&mut self, input: State<I>) -> ParseResult<Self::Output, I>
    where
        I: Stream<Item = Token>,
    {
        match input.clone().uncons_token() {
            Err(e) => Err(e),
            Ok((t, s)) => {
                if t.chars().all(|c| c.is_alphanumeric() || c == '_')
                    && !t.starts_with(|c: char| c.is_numeric())
                {
                    Ok(((t.data, input.position), s))
                } else {
                    Err(Consumed::Empty(ParseError::new(
                        input.position,
                        PError::Message(format!("Invalid identifier '{}'", t.data).into()),
                    )))
                }
            }
        }
    }
}

/// An identifier, a string of characters matching `[a-zA-Z_][a-zA-Z0-9_]*`.
fn identifier<I>() -> Identifier<I>
where
    I: Stream<Item = Token>,
{
    Identifier(PhantomData)
}

fn integer_literal<I: Stream<Item = Token>>(input: State<I>) -> ParseResult<super::Expression, I> {
    matches(
        |tok| tok.chars().all(|c| c.is_numeric()),
        Some("Integer literal contains non-numeric character(s)"),
    )
    .and_then(|s: Token| s.parse::<i8>())
    .map(super::Expression::Literal)
    .parse_state(input)
}

struct SingleExpr<I>(PhantomData<I>);
impl<I> Parser for SingleExpr<I>
where
    I: Stream<Item = Token>,
{
    type Input = I;
    type Output = super::Expression;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Expression, I> {
        let variable =
            identifier().map(|(ident, position)| super::Expression::Variable(ident, position));

        optional(literal("-"))
            .and(parser(integer_literal).or(variable))
            .map(|(neg, val)| {
                if neg.is_some() {
                    super::Expression::Negation(Box::new(val))
                } else {
                    val
                }
            })
            .parse_state(input)
    }
}

struct ArithExpr<I>(PhantomData<I>);
impl<I> Parser for ArithExpr<I>
where
    I: Stream<Item = Token>,
{
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
                "+" => super::Expression::Addition(Box::new(lhs), Box::new(rhs)),
                "-" => super::Expression::Subtraction(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            });

        arithmetic.parse_state(input)
    }
}

struct Expression<I>(PhantomData<I>);
impl<I> Parser for Expression<I>
where
    I: Stream<Item = Token>,
{
    type Input = I;
    type Output = super::Expression;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Expression, I> {
        ptry(ArithExpr(PhantomData))
            .or(SingleExpr(PhantomData))
            .parse_state(input)
    }
}

/// An expression yielding a value.
fn expression<I>() -> Expression<I>
where
    I: Stream<Item = Token>,
{
    Expression(PhantomData)
}

#[test]
fn test_bin_arith_expr() {
    assert_eq!(
        tparse(expression(), "1 + 1"),
        super::Expression::Addition(
            Box::new(super::Expression::Literal(1)),
            Box::new(super::Expression::Literal(1))
        )
    );

    assert_eq!(
        tparse(expression(), "x - y"),
        super::Expression::Subtraction(
            Box::new(super::Expression::Variable("x".to_string(), spos(1, 1))),
            Box::new(super::Expression::Variable("y".to_string(), spos(1, 4)))
        )
    );

    assert_eq!(
        tparse(expression(), "x - 1 + y"),
        super::Expression::Subtraction(
            Box::new(super::Expression::Variable("x".to_string(), spos(1, 1))),
            Box::new(super::Expression::Addition(
                Box::new(super::Expression::Literal(1)),
                Box::new(super::Expression::Variable("y".to_string(), spos(1, 8)))
            ))
        )
    );
}

#[test]
fn test_negation_expr() {
    assert_eq!(
        tparse(expression(), "-x"),
        super::Expression::Negation(Box::new(super::Expression::Variable(
            "x".to_string(),
            spos(1, 2)
        )))
    );
}

struct BooleanExpr<I>(PhantomData<I>);
impl<I> Parser for BooleanExpr<I>
where
    I: Stream<Item = Token>,
{
    type Input = I;
    type Output = super::BooleanExpr;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::BooleanExpr, I> {
        use super::BooleanExpr::*;

        expression()
            .and(choice([
                literal("=="),
                literal("!="),
                literal(">="),
                literal("<="),
                literal(">"),
                literal("<"),
            ]))
            .and(expression())
            .map(|((lhs, op), rhs)| match &op[..] {
                "==" => Equal(lhs, rhs),
                "!=" => NotEqual(lhs, rhs),
                ">=" => LessOrEqual(rhs, lhs),
                "<=" => LessOrEqual(lhs, rhs),
                ">" => Greater(lhs, rhs),
                "<" => Greater(rhs, lhs),
                _ => unreachable!(),
            })
            .parse_state(input)
    }
}

fn bool_expr<I>() -> BooleanExpr<I>
where
    I: Stream<Item = Token>,
{
    BooleanExpr(PhantomData)
}

struct Statement<I>(PhantomData<I>);
struct BlockStatement<I>(PhantomData<I>);
impl<I> Parser for BlockStatement<I>
where
    I: Stream<Item = Token>,
{
    type Input = I;
    type Output = super::Statement;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Statement, I> {
        let block = || between(literal("{"), literal("}"), many::<Vec<_>, _>(statement()));

        let conditional = literal("if")
            .with(between(literal("("), literal(")"), bool_expr()))
            .and(block())
            .and(optional(literal("else").with(block())))
            .map(|((predicate, tb), eb)| super::Statement::Conditional(predicate, tb, eb));

        let while_loop = literal("while")
            .with(between(literal("("), literal(")"), bool_expr()))
            .and(block())
            .map(|(predicate, block)| super::Statement::While(predicate, block));

        while_loop.or(conditional).parse_state(input)
    }
}

impl<I> Parser for Statement<I>
where
    I: Stream<Item = Token>,
{
    type Input = I;
    type Output = super::Statement;

    fn parse_state(&mut self, input: State<I>) -> ParseResult<super::Statement, I> {
        let position = input.position;

        let assignment = identifier()
            .skip(literal("="))
            .and(expression())
            .map(|((ident, _), expr)| super::Statement::Assignment(ident, expr, position));

        // TODO would like a diagnostic for `return = expr` since it's a common error.
        let ret = literal("return")
            .with(expression())
            .map(|expr| super::Statement::Return(expr, position));

        BlockStatement(PhantomData)
            .or(ret.or(assignment).skip(literal(";")))
            .parse_state(input)
    }
}

fn statement<I>() -> Statement<I> {
    Statement(PhantomData)
}

#[test]
#[ignore] // Expected '=' at column 4 for some reason.
fn test_declaration() {
    assert_eq!(
        tparse(statement(), "int x = 0;"),
        super::Statement::Declaration("x".into(), super::Expression::Literal(0), spos(1, 1))
    );
    // Initializers must be literal values only.
    assert!(statement()
        .parse(TokenStream::new("int x = y + z;".chars()))
        .is_err());
}

#[test]
fn test_assign_statement() {
    assert_eq!(
        tparse(statement(), "foo = 1;"),
        super::Statement::Assignment(
            "foo".to_string(),
            super::Expression::Literal(1),
            SourcePosition { line: 1, column: 1 }
        )
    );
}

#[test]
fn test_return_statement() {
    assert_eq!(
        tparse(statement(), "return 0;"),
        super::Statement::Return(super::Expression::Literal(0), spos(1, 1))
    );
}

#[test]
fn test_conditional_statement() {
    use super::BooleanExpr::*;
    use super::Expression::*;

    assert_eq!(
        tparse(statement(), "if (1 == 1) { }"),
        super::Statement::Conditional(Equal(Literal(1), Literal(1)), vec![], None)
    );
    assert_eq!(
        tparse(statement(), "if (0 != 1) { } else { }"),
        super::Statement::Conditional(NotEqual(Literal(0), Literal(1)), vec![], Some(vec![]))
    );
}

#[test]
fn test_while_statement() {
    use super::BooleanExpr::*;
    use super::Expression::*;

    assert_eq!(
        tparse(statement(), "while (i > 0) { }"),
        super::Statement::While(
            Greater(Variable("i".to_string(), spos(1, 8)), Literal(0)),
            vec![],
        )
    );
}

fn function<I>(input: State<I>) -> ParseResult<super::Function, I>
where
    I: Stream<Item = Token>,
{
    let position = input.position;

    let param_list = between(
        literal("("),
        literal(")"),
        sep_by::<Vec<_>, _, _>(ty().and(identifier()), literal(",")),
    );
    let declaration = literal("int")
        .with(identifier())
        .skip(literal("="))
        .and(parser(integer_literal))
        .skip(literal(";"))
        .map(|((ident, _), expr)| super::Statement::Declaration(ident, expr, position));

    ty().and(identifier())
        .and(param_list)
        .and(between(
            literal("{"),
            literal("}"),
            many::<Vec<_>, _>(declaration).and(many::<Vec<_>, _>(statement())),
        ))
        .map(|(((ty, (name, _)), params), (mut decls, body))| {
            // Declarations must be first in function body, but are parsed
            // as any other statement.
            decls.extend(body);
            super::Function {
                returns: ty,
                name: name,
                // Extract token text
                parameters: params,
                body: decls,
            }
        })
        .parse_state(input)
}

pub fn parse<I: Iterator<Item = char> + Clone>(i: I) -> Result<super::Function, super::Error> {
    let stream = TokenStream::new(i);
    let res = parser(function).parse(stream);

    let (f, _) = res?;
    Ok(f)
}

#[cfg(test)]
fn tparse<'a, T, P>(mut parser: P, s: &'a str) -> T
where
    P: Parser<Input = TokenStream<::std::str::Chars<'a>>, Output = T>,
{
    parser.parse(TokenStream::new(s.chars())).unwrap().0
}

#[cfg(test)]
fn spos(line: i32, col: i32) -> SourcePosition {
    SourcePosition {
        line: line,
        column: col,
    }
}
