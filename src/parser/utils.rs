use nom::IResult;

pub fn skip<I, O, E>(mut fun: impl FnMut(I) -> IResult<I, O, E>) ->
    impl FnMut(I) -> IResult<I, (), E>
{
    move |input: I| {
        fun(input).map(|(out_stream, _)| (out_stream, ()))
    }
}

pub fn keep<'a, 'b: 'a, I, O, E>(data: &'a mut Option<O>, mut fun: impl FnMut(I) -> IResult<I, O, E> + 'b) ->
    impl FnMut(I) -> IResult<I, (), E> + 'a
{
    move |input: I| {
        fun(input).map(|(out_stream, out_data)| {
            *data = Some(out_data);
            (out_stream, ())
        })
    }
}

pub fn apply<I, O, E, List: nom::sequence::Tuple<I, O, E>>(mut parsers: List) ->
    impl FnMut(I) -> IResult<I, (), E>
{
    move |input: I| {
        parsers.parse(input).map(|(out_stream, _)| {
            (out_stream, ())
        })
    }
}