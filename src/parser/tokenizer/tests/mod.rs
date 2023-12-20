use nom::{IResult, Parser};
use nom::character::complete::char;

use crate::parser::tokenizer::*;
use crate::parser::utils::*;

use super::Span;

#[test]
fn test_keyword_extraction() {
    let keywords = "if else while fn let perform with handler effect return";
    let mut keywords = Span::from(keywords);

    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::If));
    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::Else));
    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::While));
    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::Fn));
    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::Let));
    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::Perform));
    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::With));
    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::Handler));
    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::Effect));
    assert_eq!(parse(&mut keywords, parse_keyword), Ok(Keyword::Return));
}

fn parse<'a, 'b: 'a, T>(input: &'a mut Span<'b>, parser: impl FnMut(Span<'b>) -> IResult<Span<'b>, T>) -> Result<T, String> {
    let data = std::mem::replace(input, Span::from(""));
    let mut out_data = None;
    let result = apply((
        keep(&mut out_data, parser),
        skip(space0),
    ))(data);

    match result {
        Ok((mut span, _)) => {
            std::mem::swap(input, &mut span);
            Ok(out_data.unwrap())
        },
        Err(err) => Err(err.to_string()),
    }
}