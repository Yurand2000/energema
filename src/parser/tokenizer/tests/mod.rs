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

