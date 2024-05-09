use std::fmt::Debug;

#[derive(PartialEq, Debug)]
pub enum Either<T, U> {
    Left(T),
    Right(U),
}

pub struct ParseOk<'a, T> {
    result: T,
    input: &'a str,
}

impl<'a, T> ParseOk<'a, T> {
    fn new(result: T, input: &'a str) -> Self {
        ParseOk { result, input }
    }
}

pub type ParseResult<'a, T> = Result<ParseOk<'a, T>, String>;

pub trait Parse
where
    Self: Sized,
    Self::Result: PartialEq + Debug,
{
    type Result;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result>;

    fn run(&self, input: &str) -> Result<Self::Result, String> {
        self.parse(input).map(|x| x.result)
    }

    fn and<U: Parse>(self, other: U) -> And<Self, U> {
        And(self, other)
    }

    fn and_ref<'a, U: Parse>(&'a self, other: &'a U) -> AndRef<'a, Self, U> {
        AndRef(self, other)
    }

    fn or<U: Parse>(self, other: U) -> Or<Self, U> {
        Or(self, other)
    }

    fn or_ref<'a, U: Parse>(&'a self, other: &'a U) -> OrRef<'a, Self, U> {
        OrRef(self, other)
    }

    fn map<U, F: Fn(Self::Result) -> U>(self, f: F) -> Map<Self, F, U> {
        Map(self, f)
    }

    fn map_ref<'a, U, F: Fn(Self::Result) -> U>(&'a self, f: &'a F) -> MapRef<'a, Self, F, U> {
        MapRef(self, f)
    }

    fn zero_or_more(self) -> ZeroOrMoreOf<Self> {
        ZeroOrMoreOf(self)
    }

    fn zero_or_more_ref<'a>(&'a self) -> ZeroOrMoreOfRef<'a, Self> {
        ZeroOrMoreOfRef(self)
    }

    fn one_or_more(self) -> OneOrMoreOf<Self> {
        OneOrMoreOf(self)
    }

    fn one_or_more_ref<'a>(&'a self) -> OneOrMoreOfRef<'a, Self> {
        OneOrMoreOfRef(self)
    }

    fn drop<U: Parse>(self, other: U) -> Drop<Self, U> {
        Drop(self, other)
    }

    fn drop_ref<'a, U: Parse>(&'a self, other: &'a U) -> DropRef<'a, Self, U> {
        DropRef(self, other)
    }

    fn keep<U: Parse>(self, other: U) -> Keep<Self, U> {
        Keep(self, other)
    }

    fn keep_ref<'a, U: Parse>(&'a self, other: &'a U) -> KeepRef<'a, Self, U> {
        KeepRef(self, other)
    }

    fn sbws(self) -> Sbws<Self> {
        Sbws(self)
    }

    fn sbws_ref<'a>(&'a self) -> SbwsRef<'a, Self> {
        SbwsRef(self)
    }

    fn sep_by(self, sep: char) -> SepBy<Self> {
        SepBy(self, sep)
    }

    fn sep_by_ref<'a>(&'a self, sep: char) -> SepByRef<'a, Self> {
        SepByRef(self, sep)
    }

    fn parse_while<F: Fn(&Self::Result) -> bool>(self, f: F) -> ParseWhile<Self, F> {
        ParseWhile(self, f)
    }

    fn parse_while_ref<'a, F: Fn(&Self::Result) -> bool>(
        &'a self,
        f: &'a F,
    ) -> ParseWhileRef<'a, Self, F> {
        ParseWhileRef(self, f)
    }
}

pub struct NullParser;

impl Parse for NullParser {
    type Result = ();

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Result> {
        Ok(ParseOk::new((), input))
    }
}

pub struct CharParser(char);

impl Parse for CharParser {
    type Result = char;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Result> {
        match input.as_bytes().get(0) {
            Some(x) => {
                let ch = *x as char;
                if ch == self.0 {
                    Ok(ParseOk {
                        result: ch,
                        input: &input[1..],
                    })
                } else {
                    Err(format!("Expected {:?} but found {:?}", self.0, ch))
                }
            }
            None => Err(format!("No Characters left to parse")),
        }
    }
}

pub struct AnyCharParser;

impl Parse for AnyCharParser {
    type Result = char;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        match input.as_bytes().get(0) {
            Some(&ch) => Ok(ParseOk {
                result: ch as char,
                input: &input[1..],
            }),
            None => Err(format!("No Input left to parse")),
        }
    }
}

pub struct And<T: Parse, U: Parse>(T, U);

impl<T: Parse, U: Parse> Parse for And<T, U> {
    type Result = (<T as Parse>::Result, <U as Parse>::Result);

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        AndRef(&self.0, &self.1).parse(input)
    }
}
pub struct AndRef<'a, T: Parse, U: Parse>(&'a T, &'a U);

impl<'a, T: Parse, U: Parse> Parse for AndRef<'a, T, U> {
    type Result = (<T as Parse>::Result, <U as Parse>::Result);

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        let ParseOk { result: lhs, input } = self.0.parse(input)?;
        let ParseOk { result: rhs, input } = self.1.parse(input)?;
        Ok(ParseOk::new((lhs, rhs), input))
    }
}

pub struct Or<T: Parse, U: Parse>(T, U);

impl<T: Parse, U: Parse> Parse for Or<T, U> {
    type Result = Either<<T as Parse>::Result, <U as Parse>::Result>;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        OrRef(&self.0, &self.1).parse(input)
    }
}

pub struct OrRef<'a, T: Parse, U: Parse>(&'a T, &'a U);

impl<'a, T: Parse, U: Parse> Parse for OrRef<'a, T, U> {
    type Result = Either<<T as Parse>::Result, <U as Parse>::Result>;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        match self.0.parse(input) {
            Ok(ParseOk { result, input }) => Ok(ParseOk::new(Either::Left(result), input)),
            _ => match self.1.parse(input) {
                Ok(ParseOk { result, input }) => Ok(ParseOk::new(Either::Right(result), input)),
                Err(err) => Err(err),
            },
        }
    }
}

pub struct Map<T: Parse, F: Fn(<T as Parse>::Result) -> U, U>(T, F);

impl<T: Parse, U: PartialEq + Debug, F: Fn(<T as Parse>::Result) -> U> Parse for Map<T, F, U> {
    type Result = U;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        MapRef(&self.0, &self.1).parse(input)
    }
}

pub struct MapRef<'a, T: Parse, F: Fn(<T as Parse>::Result) -> U, U>(&'a T, &'a F);

impl<'a, T: Parse, U: PartialEq + Debug, F: Fn(<T as Parse>::Result) -> U> Parse
    for MapRef<'a, T, F, U>
{
    type Result = U;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        let ParseOk { result, input } = self.0.parse(input)?;
        Ok(ParseOk::new((self.1)(result), input))
    }
}

pub struct AllOf<T: Parse>(Vec<T>);

impl<T: Parse> Parse for AllOf<T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        AllOfRef(&self.0).parse(input)
    }
}

pub struct AllOfRef<'a, T: Parse>(&'a Vec<T>);

impl<'a, T: Parse> Parse for AllOfRef<'a, T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        let mut results = ParseOk::new(vec![], input);
        for parser in self.0.iter() {
            let ParseOk { result, input } = parser.parse(results.input)?;
            results.result.push(result);
            results.input = input;
        }

        Ok(results)
    }
}

pub struct OneOf<T: Parse>(Vec<T>);

impl<T: Parse> Parse for OneOf<T> {
    type Result = <T as Parse>::Result;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        OneOfRef(&self.0).parse(input)
    }
}
pub struct OneOfRef<'a, T: Parse>(&'a Vec<T>);

impl<'a, T: Parse> Parse for OneOfRef<'a, T> {
    type Result = <T as Parse>::Result;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        let mut error = "Nothing to parse".into();
        for parser in self.0.iter() {
            match parser.parse(input) {
                Ok(result) => return Ok(result),
                Err(err) => error = err,
            }
        }
        Err(error)
    }
}

pub struct ZeroOrMoreOf<T: Parse>(T);

impl<T: Parse> Parse for ZeroOrMoreOf<T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        ZeroOrMoreOfRef(&self.0).parse(input)
    }
}

pub struct ZeroOrMoreOfRef<'a, T: Parse>(&'a T);

impl<'a, T: Parse> Parse for ZeroOrMoreOfRef<'a, T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'b>(&self, mut input: &'b str) -> ParseResult<'b, Self::Result> {
        let mut results = vec![];
        while let Ok(ParseOk {
            result,
            input: input1,
        }) = self.0.parse(input)
        {
            results.push(result);
            input = input1;
        }

        Ok(ParseOk::new(results, input))
    }
}

pub struct OneOrMoreOf<T: Parse>(T);

impl<T: Parse> Parse for OneOrMoreOf<T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        OneOrMoreOfRef(&self.0).parse(input)
    }
}

pub struct OneOrMoreOfRef<'a, T: Parse>(&'a T);

impl<'a, T: Parse> Parse for OneOrMoreOfRef<'a, T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        let ParseOk { result, mut input } = self.0.parse(input)?;
        let mut results = vec![result];

        while let Ok(ParseOk {
            result,
            input: input1,
        }) = self.0.parse(input)
        {
            results.push(result);
            input = input1;
        }

        Ok(ParseOk::new(results, input))
    }
}

pub struct Drop<T: Parse, U: Parse>(T, U);

impl<T: Parse, U: Parse> Parse for Drop<T, U> {
    type Result = <U as Parse>::Result;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Result> {
        DropRef(&self.0, &self.1).parse(input)
    }
}

pub struct DropRef<'a, T: Parse, U: Parse>(&'a T, &'a U);

impl<'a, T: Parse, U: Parse> Parse for DropRef<'a, T, U> {
    type Result = <U as Parse>::Result;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        let ParseOk { input, .. } = self.0.parse(input)?;
        self.1.parse(input)
    }
}

pub struct Keep<T: Parse, U: Parse>(T, U);

impl<T: Parse, U: Parse> Parse for Keep<T, U> {
    type Result = <T as Parse>::Result;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Result> {
        KeepRef(&self.0, &self.1).parse(input)
    }
}

pub struct KeepRef<'a, T: Parse, U: Parse>(&'a T, &'a U);

impl<'a, T: Parse, U: Parse> Parse for KeepRef<'a, T, U> {
    type Result = <T as Parse>::Result;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        let ParseOk { result, input } = self.0.parse(input)?;
        let ParseOk { input, .. } = self.1.parse(input)?;
        Ok(ParseOk { result, input })
    }
}

pub struct WhitespaceParser;

impl Parse for WhitespaceParser {
    type Result = String;

    fn parse<'a>(&self, mut input: &'a str) -> ParseResult<'a, Self::Result> {
        let mut ws = String::new();
        while let Ok(ParseOk {
            result,
            input: input1,
        }) = one_of_p(" \n\t\r".chars().map(|ch| CharParser(ch))).parse(input)
        {
            ws.push(result);
            input = input1;
        }
        Ok(ParseOk { result: ws, input })
    }
}

pub struct Sbws<T: Parse>(T);

impl<T: Parse> Parse for Sbws<T> {
    type Result = <T as Parse>::Result;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Result> {
        SbwsRef(&self.0).parse(input)
    }
}

pub struct SbwsRef<'a, T: Parse>(&'a T);

impl<'a, T: Parse> Parse for SbwsRef<'a, T> {
    type Result = <T as Parse>::Result;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        let ParseOk { input, .. } = WhitespaceParser.parse(input)?;
        let ParseOk { result, input } = self.0.parse(input)?;
        let ParseOk { input, .. } = WhitespaceParser.parse(input)?;
        Ok(ParseOk { result, input })
    }
}

pub struct SepBy<T: Parse>(T, char);

impl<T: Parse> Parse for SepBy<T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Result> {
        SepByRef(&self.0, self.1).parse(input)
    }
}

pub struct SepByRef<'a, T: Parse>(&'a T, char);

impl<'b, T: Parse> Parse for SepByRef<'b, T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Result> {
        self.0
            .sbws_ref()
            .keep_ref(&CharParser(self.1).sbws())
            .zero_or_more_ref()
            .and(self.0.sbws_ref())
            .map(|(mut v, e)| {
                v.push(e);
                v
            })
            .parse(input)
    }
}

pub struct ParseSquareList<T: Parse>(T);

impl<T: Parse> Parse for ParseSquareList<T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Result> {
        ParseSquareListRef(&self.0).parse(input)
    }
}

pub struct ParseSquareListRef<'a, T: Parse>(&'a T);

impl<'a, T: Parse> Parse for ParseSquareListRef<'a, T> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        CharParser('[')
            .drop(
                self.0
                    .sep_by_ref(',')
                    .or(NullParser)
                    .map(|either| match either {
                        Either::Left(left) => left,
                        Either::Right(_) => {
                            vec![]
                        }
                    }),
            )
            .keep(CharParser(']'))
            .parse(input)
    }
}

pub struct ParseWhile<T: Parse, F: Fn(&'_ <T as Parse>::Result) -> bool>(T, F);

impl<'a, T: Parse, F: Fn(&'_ <T as Parse>::Result) -> bool> Parse for ParseWhile<T, F> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'b>(&self, input: &'b str) -> ParseResult<'b, Self::Result> {
        ParseWhileRef(&self.0, &self.1).parse(input)
    }
}

pub struct ParseWhileRef<'a, T: Parse, F: Fn(&'_ <T as Parse>::Result) -> bool>(&'a T, &'a F);

impl<'a, T: Parse, F: Fn(&'_ <T as Parse>::Result) -> bool> Parse for ParseWhileRef<'a, T, F> {
    type Result = Vec<<T as Parse>::Result>;

    fn parse<'b>(&self, mut input: &'b str) -> ParseResult<'b, Self::Result> {
        let mut results = vec![];

        while let Ok(ParseOk {
            result,
            input: input1,
        }) = self.0.parse(input)
        {
            let pred = self.1(&result);
            if pred {
                results.push(result);
                input = input1;
            } else {
                break;
            }
        }

        Ok(ParseOk::new(results, input))
    }
}

pub struct StringParser(String);

impl Parse for StringParser {
    type Result = String;

    fn parse<'b>(&self, mut input: &'b str) -> ParseResult<'b, Self::Result> {
        for ch in self.0.chars() {
            let ParseOk { input: input1, .. } = CharParser(ch).parse(input)?;
            input = input1;
        }

        Ok(ParseOk::new(self.0.clone(), input))
    }
}

pub fn char_p(ch: char) -> CharParser {
    CharParser(ch)
}

pub fn any_char_p() -> AnyCharParser {
    AnyCharParser
}

pub fn str_p(st: &str) -> StringParser {
    StringParser(st.to_string())
}

pub fn one_of_p<I: IntoIterator<Item = T>, T: Parse>(iter: I) -> OneOf<T> {
    OneOf(iter.into_iter().collect())
}

pub fn bool_p() -> impl Parse<Result = bool> {
    one_of_p([str_p("true"), str_p("false")])
        .map(|x| x.parse::<bool>().expect("Cannot convert to bool"))
}

pub fn digit() -> impl Parse<Result = char> {
    one_of_p(('0'..='9').map(|ch| CharParser(ch)))
}

pub fn uint_p() -> impl Parse<Result = u64> {
    digit().one_or_more().map(|x| {
        x.into_iter()
            .collect::<String>()
            .parse()
            .expect("This has to be an int")
    })
}

pub fn int_p() -> impl Parse<Result = i64> {
    CharParser('-').and(uint_p()).or(uint_p()).map(|x| match x {
        Either::Left((_, x)) => -(x as i64),
        Either::Right(x) => x as i64,
    })
}

pub fn float_p() -> impl Parse<Result = f64> {
    int_p()
        .and(CharParser('.'))
        .and(uint_p())
        .map(|((l, _), r)| format!("{l}.{r}").parse().expect("Cannot convert to float"))
        .or(uint_p().and(CharParser('.')).map(|(x, _)| x as f64))
        .map(|x| match x {
            Either::Left(l) => l,
            Either::Right(r) => r,
        })
        .or(uint_p())
        .map(|x| match x {
            Either::Left(l) => l,
            Either::Right(r) => r as f64,
        })
}

pub fn whitespace() -> WhitespaceParser {
    WhitespaceParser
}

pub fn list_square_p<T: Parse>(t: T) -> ParseSquareList<T> {
    ParseSquareList(t)
}

pub fn dq_str_p() -> impl Parse<Result = String> {
    char_p('"')
        .drop(any_char_p().parse_while(|&x| x != '"'))
        .keep(char_p('"'))
        .map(|x| x.into_iter().collect())
}

pub fn sq_str_p() -> impl Parse<Result = String> {
    char_p('\'')
        .drop(any_char_p().parse_while(|&x| x != '\''))
        .keep(char_p('\''))
        .map(|x| x.into_iter().collect())
}
