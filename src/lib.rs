#![feature(box_patterns)]
#![feature(box_syntax)]
use core::fmt::Display;
use std::{
    collections::VecDeque,
    ops::{BitAnd, BitOr, Shl, Shr},
};
use Either::*;

pub enum Either<T, U> {
    Left(T),
    Right(U),
}

type PString = VecDeque<char>;

type ParseResult<T> = Result<T, String>;

type _ParseResult<T> = (PString, ParseResult<T>);

pub struct Parser<T> {
    run_parser: Box<dyn FnMut(PString) -> _ParseResult<T>>,
}

// Constructors
impl<T> Parser<T> {
    pub fn new<F>(f: F) -> Parser<T>
    where
        F: FnMut(PString) -> _ParseResult<T> + 'static,
    {
        Parser { run_parser: box f }
    }
}

impl<T: Default> Parser<T> {
    pub fn empty() -> Parser<T> {
        Parser::new(|input| (input, Ok(T::default())))
    }
}

impl<T> Parser<T> {
    pub fn failed() -> Parser<T> {
        Parser::new(|input| (input, Err("Failed parser".into())))
    }
}

// Runners
impl<T> Parser<T> {
    pub fn _run(&mut self, input: PString) -> _ParseResult<T> {
        (self.run_parser)(input)
    }

    pub fn run<S: Display>(&mut self, input: S) -> ParseResult<T> {
        self._run(input.to_string().chars().collect()).1
    }

    pub fn drun<S: Display>(&mut self, input: S) -> ParseResult<T> {
        println!("Input: {}", input);
        let (input, result) = self._run(input.to_string().chars().collect());
        println!("Remaining: {:?}", input);
        result
    }
}

// Parser with Predicate
impl Parser<String> {
    pub fn parse_until<F>(mut p: F) -> Parser<String>
    where
        F: FnMut(char) -> bool + 'static,
    {
        Parser::new(move |mut input| {
            let mut string = String::new();
            loop {
                let item = input.front().cloned();
                match item {
                    Some(ch) if p(ch) => {
                        string.push(ch);
                        input.pop_front();
                    }
                    _ => break (input, Ok(string)),
                }
            }
        })
    }
}

// Mapper
impl<T: 'static> Parser<T> {
    pub fn map<U, F>(mut self, mut f: F) -> Parser<U>
    where
        F: FnMut(T) -> U + 'static,
    {
        Parser::new(move |input| {
            let (input, result) = self._run(input);
            (input, result.map(|result| f(result)))
        })
    }
}

// Bind
impl<T: 'static> Parser<T> {
    pub fn bind<U, F>(mut self, mut f: F) -> Parser<U>
    where
        F: FnMut(T) -> Parser<U> + 'static,
    {
        Parser::new(move |input| {
            let (input, result) = self._run(input);
            match result {
                Ok(t) => f(t)._run(input),
                Err(err) => (input, Err(err)),
            }
        })
    }
}

// Combinators
impl<T: 'static> Parser<T> {
    fn _and<U: 'static>(mut self, mut other: Parser<U>) -> Parser<(T, U)> {
        Parser::new(move |input| {
            let (input1, r1) = self._run(input.clone());
            match r1 {
                Ok(t) => {
                    let (input2, r2) = other._run(input1);
                    match r2 {
                        Ok(u) => (input2, Ok((t, u))),
                        Err(err) => (input, Err(err)),
                    }
                }
                Err(err) => (input1, Err(err)),
            }
        })
    }

    fn _or<U: 'static>(mut self, mut other: Parser<U>) -> Parser<Either<T, U>> {
        Parser::new(move |input| {
            let (input, r1) = self._run(input);
            match r1 {
                Ok(t) => (input, Ok(Left(t))),
                Err(_) => {
                    let (input, r2) = other._run(input);
                    (
                        input,
                        match r2 {
                            Ok(u) => Ok(Right(u)),
                            Err(err) => Err(err),
                        },
                    )
                }
            }
        })
    }

    fn _drop_fst<U: 'static>(mut self, mut other: Parser<U>) -> Parser<U> {
        Parser::new(move |input| {
            let (input1, r1) = self._run(input.clone());
            match r1 {
                Ok(_) => {
                    let (input2, r2) = other._run(input1);
                    match r2 {
                        Ok(u) => (input2, Ok(u)),
                        Err(err) => (input, Err(err)),
                    }
                }
                Err(err) => (input1, Err(err)),
            }
        })
    }

    fn _drop_snd<U: 'static>(mut self, mut other: Parser<U>) -> Parser<T> {
        Parser::new(move |input| {
            let (input1, r1) = self._run(input.clone());
            match r1 {
                Ok(t) => {
                    let (input2, r2) = other._run(input1);
                    match r2 {
                        Ok(_) => (input2, Ok(t)),
                        Err(err) => (input, Err(err)),
                    }
                }
                Err(err) => (input1, Err(err)),
            }
        })
    }
}

impl<T: 'static, U: 'static> BitAnd<Parser<U>> for Parser<T> {
    type Output = Parser<(T, U)>;

    fn bitand(self, rhs: Parser<U>) -> Self::Output {
        self._and(rhs)
    }
}

impl<T: 'static, U: 'static> BitOr<Parser<U>> for Parser<T> {
    type Output = Parser<Either<T, U>>;

    fn bitor(self, rhs: Parser<U>) -> Self::Output {
        self._or(rhs)
    }
}

impl<T: 'static, U: 'static> Shr<Parser<U>> for Parser<T> {
    type Output = Parser<U>;

    fn shr(self, rhs: Parser<U>) -> Self::Output {
        self._drop_fst(rhs)
    }
}

impl<T: 'static, U: 'static> Shl<Parser<U>> for Parser<T> {
    type Output = Parser<T>;

    fn shl(self, rhs: Parser<U>) -> Self::Output {
        self._drop_snd(rhs)
    }
}

// Parsers
pub fn parse_char(ch: char) -> Parser<char> {
    Parser::new(move |mut input| {
        let front = input.front().cloned();
        match front {
            Some(x) if x == ch => {
                input.pop_front();
                (input, Ok(ch))
            }
            Some(y) => (input, Err(format!("Expected {ch:?} got {y}"))),
            None          => (input, Err(format!("Expected {ch:?} got empty string")))
        }
    })
}

pub fn parse_str(string: &str) -> Parser<String> {
    parse_list(string.chars().map(|ch| parse_char(ch)))
}

pub fn parse_bool() -> Parser<bool> {
    parse_one_of([parse_str("true"), parse_str("false")]).map(|x| match x.as_str() {
        "true" => true,
        "false" => false,
        _ => panic!("Bool cannot be other than true and false"),
    })
}

pub fn parse_one_of<T: 'static, I: IntoIterator<Item = Parser<T>>>(list: I) -> Parser<T> {
    list.into_iter()
        .reduce(|p1, p2| {
            (p1 | p2).map(|r| match r {
                Left(v1) => v1,
                Right(v2) => v2,
            })
        })
        .unwrap_or(Parser::failed())
}

pub fn parse_list<T: 'static, I: IntoIterator<Item = Parser<T>>, C: FromIterator<T>>(
    list: I,
) -> Parser<C> {
    list.into_iter()
        .fold(Parser::empty(), |p1: Parser<Vec<T>>, p2| {
            (p1 & p2).map(|(mut c, t)| {
                c.push(t);
                c
            })
        })
        .map(|c| c.into_iter().collect())
}

pub fn parse_while_true<F>(f: F) -> Parser<String>
where
    F: FnMut(char) -> bool + 'static,
{
    Parser::parse_until(f)
}

pub fn parse_uints() -> Parser<String> {
    parse_while_true(|ch| ch.is_digit(10)).bind(|x| {
        Parser::new(move |input| {
            if x.is_empty() {
                (input, Err(format!("Expected digit got {x:?}")))
            } else {
                (input, Ok(x.clone()))
            }
        })
    })
}

pub fn parse_uint() -> Parser<usize> {
    parse_uints().map(|x| x.parse().expect("u64: This will never fail!"))
}

pub fn parse_ints() -> Parser<String> {
    parse_one_of([parse_list([parse_str("-"), parse_uints()]), parse_uints()])
}

pub fn parse_int() -> Parser<i64> {
    parse_ints().map(|x| x.parse().expect("i64: This will never fail!"))
}

pub fn parse_float() -> Parser<f64> {
    parse_one_of([
        parse_list([
            parse_ints(),
            parse_str("."),
            parse_one_of([parse_uints(), parse_white_space()])
        ]),
        parse_ints()
    ])
    .map(|x: String| x.parse().expect("f64: This will never fail!"))
}

pub fn parse_string() -> Parser<String> {
    parse_char('"') >> parse_while_true(|ch| ch != '"') << parse_char('"')
}

pub fn parse_white_space() -> Parser<String> {
    parse_while_true(|ch| ch.is_whitespace())
}

pub fn parse_zero_or_more<T: 'static>(mut p: Parser<T>) -> Parser<Vec<T>> {
    Parser::new(move |mut input| {
        let mut r;
        let mut list = Vec::new();
        loop {
            (input, r) = p._run(input);
            if let Ok(t) = r {
                list.push(t);
            } else {
                break (input, Ok(list));
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_char() {
        assert_eq!(parse_char('a').run("abcd"), Ok('a'));
        assert_ne!(parse_char('a').run("bcd"), Ok('a'));
    }

    #[test]
    fn test_parse_str() {
        assert_eq!(parse_str("null").run("null"), Ok("null".into()));
        assert_ne!(parse_str("true").run("false"), Ok("true".into()))
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse_bool().run("true"), Ok(true));
        assert_eq!(parse_bool().run("false"), Ok(false));
        assert_ne!(parse_bool().run("False"), Ok(false));
    }

    #[test]
    fn test_parse_float() {
        assert_eq!(parse_float().run("123"), Ok(123f64));
        assert_eq!(parse_float().run("-123.43"), Ok(-123.43));
        assert_eq!(parse_float().run("0.23"), Ok(0.23));
        assert_eq!(parse_float().run("23."), Ok(23.));
    }
}
