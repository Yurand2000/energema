use nom::IResult;

pub enum ParserOption<T> {
    PNone,
    Some(T)
}

pub fn skip<I, O, E>(mut fun: impl FnMut(I) -> IResult<I, O, E>) ->
    impl FnMut(I) -> IResult<I, (), E>
{
    move |input: I| {
        fun(input).map(|(out_stream, _)| (out_stream, ()))
    }
}

pub fn keep<'a, 'b: 'a, I, O, E>(data: &'a mut ParserOption<O>, mut fun: impl FnMut(I) -> IResult<I, O, E> + 'b) ->
    impl FnMut(I) -> IResult<I, (), E> + 'a
{
    move |input: I| {
        fun(input).map(|(out_stream, out_data)| {
            *data = ParserOption::Some(out_data);
            (out_stream, ())
        })
    }
}

pub fn has_success<'a, 'b: 'a, I, O, E>(data: &'a mut bool, mut fun: impl FnMut(I) -> IResult<I, O, E> + 'b) ->
    impl FnMut(I) -> IResult<I, (), E> + 'a
{
    *data = false;
    move |input: I| {
        fun(input).map(|(out_stream, _)| {
            *data = true;
            (out_stream, ())
        })
    }
}

pub fn opt_success<'a, 'b: 'a, I, O, E>(data: &'a mut bool, mut fun: impl FnMut(I) -> IResult<I, Option<O>, E> + 'b) ->
    impl FnMut(I) -> IResult<I, (), E> + 'a
{
    *data = false;
    move |input: I| {
        fun(input).map(|(out_stream, res)| {
            *data = res.is_some();
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

pub fn first<I, O1, O2, E, F1, F2>(mut p1: F1, mut p2: F2) -> impl FnMut(I) -> IResult<I, O1, E>
    where F1: FnMut(I) -> IResult<I, O1, E>, F2: FnMut(I) -> IResult<I, O2, E>
{
    move |input: I| {
        let (next, data) = p1(input)?;
        let (out, _) = p2(next)?;

        Ok((out, data))
    }
}

pub fn second<I, O1, O2, E, F1, F2>(mut p1: F1, mut p2: F2) -> impl FnMut(I) -> IResult<I, O2, E>
where F1: FnMut(I) -> IResult<I, O1, E>, F2: FnMut(I) -> IResult<I, O2, E>
{
    move |input: I| {
        let (next, _) = p1(input)?;
        let (out, data) = p2(next)?;

        Ok((out, data))
    }
}

impl<T> ParserOption<T> {
    pub fn take(self) -> T {
        self.to_option().unwrap()
    }

    pub fn to_option(self) -> Option<T> {
        match self {
            ParserOption::PNone => None,
            ParserOption::Some(val) => Some(val),
        }
    }
}