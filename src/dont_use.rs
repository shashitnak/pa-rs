use core::marker::PhantomData;
use std::iter::Map;

enum Node {
    Null,
    Leaf(u8),
    And(Box<Node>, Box<Node>),
    Or(Box<Node>, Box<Node>),
    Dl(Box<Node>, Box<Node>),
    Dr(Box<Node>, Box<Node>),
    P(Box<dyn Fn(u8) -> bool + 'static>),
    Tuf(Box<Node>)
}

#[derive(Debug, PartialEq)]
pub enum Ret {
    NoVal,
    Char(u8),
    Val(Box<Ret>),
    Tup(Box<Ret>, Box<Ret>),
}

use Ret::*;

// impl Ret {
//     fn len(&self) -> usize {
//         match self {
//             NoVal => 0,
//             Char(_) => 1,
//             Val(box val) => val.len(),
//             Tup(box lhs, box rhs) => lhs.len() + rhs.len()
//         }
//     }
// }

use Node::*;
use core::ops::{BitOr, BitAnd, Shl, Shr};

impl Node {
    pub fn run(&self, input: &str) -> Option<Ret> {
        self._run(input.as_bytes(), &mut 0)
    }

    fn _run(&self, input: &[u8], index: &mut usize) -> Option<Ret> {
        match self {
            Null => Some(NoVal),
            Leaf(ch) => {
                let result = input
                    .get(*index)
                    .filter(|x| **x == *ch)
                    .map(|x| Char(*x));
                *index += 1;
                result
            },
            And(box lhs, box rhs) => {
                let x = lhs._run(input, index)?;
                let y = rhs._run(input, index)?;
                Some(Tup(box x, box y))
            },
            Or(box lhs, box rhs) => {
                lhs._run(input, index)
                    .map(|x| Val(box x))
                    .or_else(|| rhs._run(input, index).map(|x| Val(box x)))
            },
            Dl(box lhs, box rhs) => {
                lhs._run(input, index)?;
                rhs._run(input, index).map(|x| Val(box x))
            },
            Dr(box lhs, box rhs) => {
                let x = lhs._run(input, index)?;
                rhs._run(input, index)?;
                Some(Val(box x))
            },
            P(f) => {
              Some(input
                .iter()
                .take_while(|&&ch| f(ch))
                .fold(NoVal, |rest, ch| Tup(box Char(*ch), box rest)))
            },
            Tuf(box p) => {
              let mut result = NoVal;
              while let Some(ret) = p._run(input, index) {
                result = Tup(box ret, box result);
              }
              Some(result)
            }
        }
    }
}

impl BitOr for Node {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Or(box self, box rhs)
    }
}

impl BitAnd for Node {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        And(box self, box rhs)
    }
}

impl Shr for Node {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        Dl(box self, box rhs)
    }
}

impl Shl for Node {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        Dr(box self, box rhs)
    }
}

impl TryFrom<Ret> for u8 {
    type Error = &'static str;

    fn try_from(val: Ret) -> Result<u8, Self::Error> {
        match val {
            NoVal => Err("Empty value cannot be converted to char!"),
            Char(ch) | Val(box Char(ch)) => Ok(ch),
            _ => Err("Collection of values cannot be converted to a single char!")
        }
    }
}

impl TryFrom<Ret> for char {
    type Error = &'static str;

    fn try_from(val: Ret) -> Result<char, Self::Error> {
        let b: u8 = val.try_into()?;
        Ok(b as char)
    }
}

impl TryFrom<Ret> for String {
    type Error = &'static str;

    fn try_from(val: Ret) -> Result<String, Self::Error> {
        match val {
            NoVal => Ok("".into()),
            Char(ch) | Val(box Char(ch)) => Ok((ch as char).into()),
            Val(box ret) => ret.try_into(),
            Tup(box lhs, box rhs) => {
                let fhalf: String = lhs.try_into()?;
                let shalf: String = rhs.try_into()?;
                Ok(fhalf + &shalf)
            }
        }
    }
}

impl TryFrom<Ret> for bool {
  type Error = &'static str;

  fn try_from(val: Ret) -> Result<bool, Self::Error> {
    let result: String = val.try_into()?;

    match result.as_str() {
      "true" => Ok(true),
      "false" => Ok(false),
      x => Err("Cannot convert to bool")
    }
  }
}

impl<T: TryFrom<Ret>> TryFrom<Ret> for Vec<T> {
  type Error = <T as TryFrom<Ret>>::Error;

  fn try_from(mut val: Ret) -> Result<Vec<T>, Self::Error> {
    let mut result = vec![];
    while let Tup(box x, box xs) = val {
      result.push(x.try_into()?);
      val = xs;
    }
    Ok(result)
  }
}

impl TryFrom<Ret> for i64 {
  type Error = &'static str;

  fn try_from(val: Ret) -> Result<i64, Self::Error> {
    let st: String = val.try_into()?;
    st.parse().map_err(|_| "Unable to parse as a valid integer")
  }
}

pub struct Parser<T: TryFrom<Ret>>(Node, PhantomData<T>);

impl<T: TryFrom<Ret>> Parser<T>
{
    fn new(node: Node) -> Self {
        Parser(node, PhantomData)
    }

    pub fn lift<U: TryFrom<Ret>>(self) -> Parser<U> {
      Parser::<U>::new(self.0)
    }

    pub fn run(&self, input: &str) -> Option<Result<T, <T as TryFrom<Ret>>::Error>> {
        self.0
            .run(input)
            .map(|x| x.try_into())
    }
}


pub fn char_p(ch: char) -> Parser<char> {
    Parser::new(Leaf(ch as u8))
}


pub fn one_of_p<T, I>(iter: I) -> Parser<T>
  where
    T: TryFrom<Ret>,
    I: IntoIterator<Item=Parser<T>>
{
  Parser::new(
    iter
            .into_iter()
            .fold(Null,
              move |rest, Parser(node, _)| Or(box node, box rest))
  )
}


pub fn all_of_p<T, I>(iter: I) -> Parser<Vec<T>>
  where
    T: TryFrom<Ret>,
    I: IntoIterator<Item=Parser<T>>,
{
  Parser::new(
    iter
            .into_iter()
            .fold(Null,
              move |rest, Parser(node, _)| And(box node, box rest))
  )
}


pub fn zero_or_more_p<T, C>(Parser(p, _): Parser<T>) -> Parser<Vec<T>>
  where
    T: TryFrom<Ret>,
{
  Parser::new(Tuf(box p))
}


pub fn till_p<F>(f: F) -> Parser<String>
where
  F: Fn(u8) -> bool + 'static
{
  Parser::new(P(box f))
}


pub fn str_p(st: &str) -> Parser<String> {
  Parser::new(
    st
            .chars()
            .fold(Null,
              move |rest, ch| And(box Leaf(ch as u8), box rest))
  )
}

pub fn bool_p() -> Parser<bool> {
  let p = one_of_p([str_p("true"), str_p("false")]);
  p.lift()
}

pub fn digit_p() -> Parser<char> {
  one_of_p(('0'..'9').map(|x| Parser::new(Leaf(x as u8))))
}

// pub fn int_p() -> Parser<i64> {
//   (digit_p() & zero_or_more_p(digit_p())).lift()
// }