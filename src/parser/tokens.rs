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

//TokenStream and Token data types
pub struct TokenStream<'a, T> {
    stream:  &'a [T]
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct LocatedToken {
    token: Token,
    line: u32,
    column: u32
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
}

//Traits to implement for the TokenStream type
impl<'a, T> TokenStream<'a, T> {
    pub fn new(stream: &'a [T]) -> Self {
        Self { stream }
    }
}

//Traits to implement for the Token types
impl AsChar for Token {
    fn as_char(self) -> char { panic!("Token should not be seen as single characters") }
    fn is_alpha(self) -> bool { panic!("Token should not be seen as single characters") }
    fn is_alphanum(self) -> bool { panic!("Token should not be seen as single characters") }
    fn is_dec_digit(self) -> bool { panic!("Token should not be seen as single characters") }
    fn is_hex_digit(self) -> bool { panic!("Token should not be seen as single characters") }
    fn is_oct_digit(self) -> bool { panic!("Token should not be seen as single characters") }
    fn len(self) -> usize { 1 }
}

impl AsChar for LocatedToken {
    fn as_char(self) -> char { panic!("Token should not be seen as single characters") }
    fn is_alpha(self) -> bool { panic!("Token should not be seen as single characters") }
    fn is_alphanum(self) -> bool { panic!("Token should not be seen as single characters") }
    fn is_dec_digit(self) -> bool { panic!("Token should not be seen as single characters") }
    fn is_hex_digit(self) -> bool { panic!("Token should not be seen as single characters") }
    fn is_oct_digit(self) -> bool { panic!("Token should not be seen as single characters") }
    fn len(self) -> usize { 1 }
}