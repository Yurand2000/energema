use nom::{IResult, Parser};
use nom::character::complete::char;

use crate::parser::tokenizer::*;
use crate::parser::utils::*;

use super::Span;

#[test]
fn test_keyword_extraction() {
    let keywords = "if else while fn let perform with handler effect return";
    let mut keywords = StatefulParser::new(keywords);

    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::If));
    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::Else));
    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::While));
    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::Fn));
    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::Let));
    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::Perform));
    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::With));
    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::Handler));
    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::Effect));
    assert_eq!(keywords.parse(parse_keyword), Ok(Keyword::Return));
    assert_eq!(keywords.parse(skip(eof)), Ok(()));
}

#[test]
fn test_symbol_extraction() {
    let symbols = "( ) [ ] { } , : ; ~ -> ^ ! + - * / % && || ^^ | = == != > >= < <=";
    let mut symbols = StatefulParser::new(symbols);
    
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::OpenParenthesis));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::CloseParentesis));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::OpenBracket));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::CloseBracket));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::OpenBrace));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::CloseBrace));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Comma));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Colon));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Semicolon));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Tilde));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Arrow));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::NegatedSet));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Exclamation));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Plus));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Minus));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Times));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::ForwardSlash));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Modulo));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::LogicalAnd));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::LogicalOr));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::LogicalXor));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Pipe));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::Equal));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::DoubleEqual));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::NotEqual));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::GreaterThan));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::GreaterOrEqual));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::SmallerThan));
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::SmallerOrEqual));
    assert_eq!(symbols.parse(skip(eof)), Ok(()));
}

struct StatefulParser<'a> {
    span: Span<'a>
}

impl<'a> StatefulParser<'a> {
    fn new(data: &'a str) -> Self {
        Self { span: Span::from(data) }
    }

    fn parse<T>(&mut self, parser: impl FnMut(Span<'a>) -> IResult<Span<'a>, T>) -> Result<T, String>
    {
        let data = std::mem::replace(&mut self.span, Span::from(""));
        let mut out_data = None;
        let result = apply((
            keep(&mut out_data, parser),
            skip(space0),
        ))(data);
    
        match result {
            Ok((mut span, _)) => {
                std::mem::swap(&mut self.span, &mut span);
                Ok(out_data.unwrap())
            },
            Err(err) => Err(err.to_string()),
        }
    }
}

