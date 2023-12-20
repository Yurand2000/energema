use nom::*;

//Most important data type which holds the contents of each token.

#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    UnitLiteral,
    BoolLiteral(bool),
    I32Literal(i32),
    RuneLiteral(char),
    StringLiteral(String),
    Eof,
}

#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum Keyword {
    If, Else, While
}

//TokenStream and Token data types
pub struct TokenStream<'a, T> {
    stream:  &'a [T]
}

#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct LocatedToken {
    token: Token,
    line: i32,
    column: i32
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