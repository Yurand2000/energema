use std::{fmt::Debug, ops::{Index, Range}};

use nom::*;

//Most important data type which holds the contents of each token.

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Symbol(Symbol),
    UnitLiteral,            // unit
    BoolLiteral(bool),      // true | false
    I32Literal(i32),
    RuneLiteral(char),
    StringLiteral(String),
    Eof,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum Keyword {
    If,
    Else,
    While,
    Fn,
    Let,
    Perform,
    With,
    Handler,
    Effect,
    Return,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum Symbol {
    OpenParenthesis,    // (
    CloseParentesis,    // )
    OpenBracket,        // [
    CloseBracket,       // ]
    OpenBrace,          // {
    CloseBrace,         // }
    Comma,              // ,
    Colon,              // :
    Semicolon,          // ;
    Tilde,              // ~
    Arrow,              // ->
    DoubleArrow,        // =>
    NegatedSet,         // ^
    Exclamation,        // !
    Plus,               // +
    Minus,              // -
    Times,              // *
    ForwardSlash,       // /
    Modulo,             // %
    LogicalAnd,         // &&
    LogicalOr,          // ||
    LogicalXor,         // ^^
    Pipe,               // |
    Equal,              // =
    DoubleEqual,        // ==
    NotEqual,           // !=
    GreaterThan,        // >
    GreaterOrEqual,     // >=
    SmallerThan,        // <
    SmallerOrEqual,     // <=
}

impl From<Symbol> for TokenType {
    fn from(value: Symbol) -> Self {
        TokenType::Symbol(value)
    }
}

impl From<Keyword> for TokenType {
    fn from(value: Keyword) -> Self {
        TokenType::Keyword(value)
    }
}

impl From<Symbol> for Token {
    fn from(value: Symbol) -> Self {
        Token::Symbol(value)
    }
}

impl From<Keyword> for Token {
    fn from(value: Keyword) -> Self {
        Token::Keyword(value)
    }
}

#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct LocatedToken {
    token: Token,
    line: u32,
    column: u32
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum TokenType {
    Identifier,
    Keyword(Keyword),
    Symbol(Symbol),
    UnitLiteral,
    BoolLiteral,
    I32Literal,
    RuneLiteral,
    StringLiteral,
    Eof,
}

impl LocatedToken {
    pub fn new(token: Token, line: u32, column: u32) -> Self {
        Self { token, line, column }
    }

    pub fn from_span<'a>(token: Token, position: nom_locate::LocatedSpan<&'a str>) -> Self {
        Self {
            token,
            line: position.location_line(),
            column: position.get_utf8_column() as u32
        }
    }

    pub fn get_token(&self) -> &Token {
        &self.token
    }

    pub fn get_line(&self) -> u32 {
        self.line
    }

    pub fn get_column(&self) -> u32 {
        self.column
    }
}

impl PartialEq<Token> for LocatedToken {
    fn eq(&self, other: &Token) -> bool {
        &self.token == other
    }
}

impl PartialEq<TokenType> for Token {
    fn eq(&self, other: &TokenType) -> bool {
        match (self, other) {
            (Token::UnitLiteral, TokenType::UnitLiteral) |
            (Token::BoolLiteral(_), TokenType::BoolLiteral) |
            (Token::I32Literal(_), TokenType::I32Literal) |
            (Token::RuneLiteral(_), TokenType::RuneLiteral) |
            (Token::StringLiteral(_), TokenType::StringLiteral) |
            (Token::Identifier(_), TokenType::Identifier) |
            (Token::Eof, TokenType::Eof) => true,
            (Token::Keyword(k1), TokenType::Keyword(k2)) if k1 == k2 => true,
            (Token::Symbol(s1), TokenType::Symbol(s2)) if s1 == s2 => true,
            _ => false,
        }
    }
}

impl PartialEq<TokenType> for LocatedToken {
    fn eq(&self, other: &TokenType) -> bool {
        &self.token == other
    }
}

impl std::fmt::Debug for LocatedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("[{:3}:{:3} -> {:?}]", self.line, self.column, self.token, ))
    }
}

#[derive(Clone)]
pub struct TokenStream<'a, T> {
    stream: &'a [T]
}

impl<'a, T> std::fmt::Debug for TokenStream<'a, T>
    where T: std::fmt::Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.stream).finish()
    }
}

impl<'a, T> TokenStream<'a, T> {
    pub fn new(stream: &'a [T]) -> Self {
        Self { stream }
    }
}

impl<'a, T> InputTake for TokenStream<'a, T> {
    fn take(&self, count: usize) -> Self {
        Self::new( &self.stream[..count] )
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (left, right) = self.stream.split_at(count);
        (Self::new(right), Self::new(left))
    }
}

impl<'a, T> InputLength for TokenStream<'a, T> {
    fn input_len(&self) -> usize {
        self.stream.len()
    }
}

impl<'a, T> Index<usize> for TokenStream<'a, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.stream[index]
    }
}

impl<'a, T> Index<Range<usize>> for TokenStream<'a, T> {
    type Output = [T];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.stream[index]
    }
}

impl<'a, T: Clone> InputIter for TokenStream<'a, T> {
    type Item = T;

    type Iter = std::iter::Enumerate<Self::IterElem>;

    type IterElem = std::iter::Cloned<std::slice::Iter<'a, Self::Item>>;

    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.stream.iter().cloned()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
        where P: Fn(Self::Item) -> bool
    {
        self.stream.iter()
            .position(|elem| predicate(elem.clone()))
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.stream.len() >= count {
            Ok(count)
          } else {
            Err(nom::Needed::new(count - self.stream.len()))
          }
    }
}

impl<'a, 'b, T1, T2> Compare<TokenStream<'b, T2>> for TokenStream<'a, T1>
    where T1: PartialEq<T2>
{
    fn compare(&self, tokens: TokenStream<'b, T2>) -> nom::CompareResult {
        if self.stream.len() < tokens.stream.len() {
            return nom::CompareResult::Error;
        }

        let equal = self.stream.iter().zip(tokens.stream.iter())
            .all(|(stream_token, token)| stream_token == token);

        if equal { nom::CompareResult::Ok }
        else { nom::CompareResult::Error }
    }

    fn compare_no_case(&self, tokens: TokenStream<'b, T2>) -> nom::CompareResult {
        self.compare(tokens)
    }
}

#[macro_export]
macro_rules! tokens {
    ( $( $x:expr ),* ) => {
        {
            TokenStream::<TokenType>::new(&[
            $(
                $x.into(),
            )*
            ])
        }
    };
}