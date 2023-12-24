use nom::IResult;
use nom::character::complete::char;

use crate::parser::tokenizer::*;

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
    let symbols = "( ) [ ] { } , : ; ~ -> => ^ ! + - * / % && || ^^ | = == != > >= < <=";
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
    assert_eq!(symbols.parse(parse_symbol), Ok(Symbol::DoubleArrow));
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

#[test]
fn test_space_parser() {
    let data = " \t\r\n A //comment here \r\n A /*comment 2 */ A";
    let mut data = StatefulParser::new(data);

    assert_eq!(data.parse_no_space(skip(space_parser0)), Ok(()));
    assert_eq!(data.parse_no_space(skip(char('A'))), Ok(()));
    assert_eq!(data.parse_no_space(skip(space_parser1)), Ok(()));
    assert_eq!(data.parse_no_space(skip(char('A'))), Ok(()));
    assert_eq!(data.parse_no_space(skip(space_parser1)), Ok(()));
    assert_eq!(data.parse_no_space(skip(char('A'))), Ok(()));
    assert_eq!(data.parse_no_space(skip(eof)), Ok(()));
}

#[test]
fn test_token_extraction() {
    let data = "unit true false 25 my_identifier -170 if != effect 'c' \"string\"";
    let mut data = StatefulParser::new(data);

    assert_eq!(data.parse(extract_token), Ok(Token::UnitLiteral));
    assert_eq!(data.parse(extract_token), Ok(Token::BoolLiteral(true)));
    assert_eq!(data.parse(extract_token), Ok(Token::BoolLiteral(false)));
    assert_eq!(data.parse(extract_token), Ok(Token::I32Literal(25)));
    assert_eq!(data.parse(extract_token), Ok(Token::Identifier("my_identifier".to_owned())));
    assert_eq!(data.parse(extract_token), Ok(Token::I32Literal(-170)));
    assert_eq!(data.parse(extract_token), Ok(Token::Keyword(Keyword::If)));
    assert_eq!(data.parse(extract_token), Ok(Token::Symbol(Symbol::NotEqual)));
    assert_eq!(data.parse(extract_token), Ok(Token::Keyword(Keyword::Effect)));
    assert_eq!(data.parse(extract_token), Ok(Token::RuneLiteral('c')));
    assert_eq!(data.parse(extract_token), Ok(Token::StringLiteral("string".to_owned())));
    assert_eq!(data.parse(skip(eof)), Ok(()));
}

#[test]
fn test_located_token_parser() {
    let data = "fn main\r\n  let i = 0; //comment\r\n  25";
    let mut data = StatefulParser::new(data);

    assert_eq!(data.parse_no_space(skip(space_parser0)), Ok(()));
    assert_eq!(data.parse_no_space(next_token), Ok(LocatedToken::new(Token::Keyword(Keyword::Fn), 1, 1)));
    assert_eq!(data.parse_no_space(skip(space_parser0)), Ok(()));
    assert_eq!(data.parse_no_space(next_token), Ok(LocatedToken::new(Token::Identifier("main".to_owned()), 1, 4)));
    assert_eq!(data.parse_no_space(skip(space_parser0)), Ok(()));
    assert_eq!(data.parse_no_space(next_token), Ok(LocatedToken::new(Token::Keyword(Keyword::Let), 2, 3)));
    assert_eq!(data.parse_no_space(skip(space_parser0)), Ok(()));
    assert_eq!(data.parse_no_space(next_token), Ok(LocatedToken::new(Token::Identifier("i".to_owned()), 2, 7)));
    assert_eq!(data.parse_no_space(skip(space_parser0)), Ok(()));
    assert_eq!(data.parse_no_space(next_token), Ok(LocatedToken::new(Token::Symbol(Symbol::Equal), 2, 9)));
    assert_eq!(data.parse_no_space(skip(space_parser0)), Ok(()));
    assert_eq!(data.parse_no_space(next_token), Ok(LocatedToken::new(Token::I32Literal(0), 2, 11)));
    assert_eq!(data.parse_no_space(skip(space_parser0)), Ok(()));
    assert_eq!(data.parse_no_space(next_token), Ok(LocatedToken::new(Token::Symbol(Symbol::Semicolon), 2, 12)));
    assert_eq!(data.parse_no_space(skip(space_parser0)), Ok(()));
    assert_eq!(data.parse_no_space(next_token), Ok(LocatedToken::new(Token::I32Literal(25), 3, 3)));
    assert_eq!(data.parse_no_space(skip(eof)), Ok(()));
}

#[test]
fn test_tokenizer() {
    let data = "fn main\r\n  let i = 0; //comment\r\n  25";
    let out_tokens = vec![
        LocatedToken::new(Token::Keyword(Keyword::Fn), 1, 1),
        LocatedToken::new(Token::Identifier("main".to_owned()), 1, 4),
        LocatedToken::new(Token::Keyword(Keyword::Let), 2, 3),
        LocatedToken::new(Token::Identifier("i".to_owned()), 2, 7),
        LocatedToken::new(Token::Symbol(Symbol::Equal), 2, 9),
        LocatedToken::new(Token::I32Literal(0), 2, 11),
        LocatedToken::new(Token::Symbol(Symbol::Semicolon), 2, 12),
        LocatedToken::new(Token::I32Literal(25), 3, 3),
        LocatedToken::new(Token::Eof, 3, 5),
    ];

    assert_eq!(tokenize(data), Ok(out_tokens));
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

    fn parse_no_space<T>(&mut self, parser: impl FnMut(Span<'a>) -> IResult<Span<'a>, T>) -> Result<T, String>
    {
        let data = std::mem::replace(&mut self.span, Span::from(""));
        let mut out_data = None;
        let result = apply((
            keep(&mut out_data, parser),
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

