use std::hash::Hash;
use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::digit1,
    error::context,
    multi::{length_count, many_till},
    sequence::{pair, preceded, terminated},
    Finish, IResult, Parser,
};
use num_bigint::{BigInt, Sign};

#[derive(Debug)]
pub enum Value {
    Boolean(bool),
    Float(f32),
    Double(f64),
    Integer(BigInt),
    Binary(Vec<u8>),
    String(String),
    Symbol(String),
    Dictionary(Vec<(Self, Self)>),
    Sequence(Vec<Self>),
    Record { label: Box<Self>, fields: Vec<Self> },
    Set(Vec<Self>),
}

impl Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.to_vec().cmp(other.to_vec().as_ref())
    }

    pub fn to_vec(&self) -> Vec<u8> {
        match self {
            Value::Boolean(true) => [b't'].to_vec(),
            Value::Boolean(false) => [b'f'].to_vec(),
            Value::Float(f) => [[b'F'].as_slice(), f.to_be_bytes().as_slice()].concat(),
            Value::Double(d) => [[b'D'].as_slice(), d.to_be_bytes().as_slice()].concat(),
            Value::Integer(big_int) => {
                let suffix = if big_int.sign() == Sign::Minus {
                    "-"
                } else {
                    "+"
                };
                format!("{}{}", big_int.magnitude().to_str_radix(10), suffix)
                    .as_bytes()
                    .to_vec()
            }
            Value::Binary(b) => [format!("{}:", b.len()).as_bytes(), b].concat(),
            Value::String(s) => {
                [format!("{}\"", s.as_bytes().len()).as_bytes(), s.as_bytes()].concat()
            }
            Value::Symbol(s) => {
                [format!("{}'", s.as_bytes().len()).as_bytes(), s.as_bytes()].concat()
            }
            Value::Dictionary(d) => [
                [b'{'].as_slice(),
                d.iter()
                    .map(|(k, v)| vec![k.to_vec(), v.to_vec()].concat())
                    .collect::<Vec<Vec<u8>>>()
                    .concat()
                    .as_slice(),
                [b'}'].as_slice(),
            ]
            .concat(),
            Value::Sequence(s) => [
                [b'['].as_slice(),
                s.iter()
                    .map(|v| v.to_vec())
                    .collect::<Vec<Vec<u8>>>()
                    .concat()
                    .as_slice(),
                [b']'].as_slice(),
            ]
            .concat(),
            Value::Record { label, fields } => [
                [b'<'].as_slice(),
                label.to_vec().as_slice(),
                fields
                    .iter()
                    .map(|v| v.to_vec())
                    .collect::<Vec<Vec<u8>>>()
                    .concat()
                    .as_slice(),
                [b'>'].as_slice(),
            ]
            .concat(),
            Value::Set(s) => [
                [b'#'].as_slice(),
                s.iter()
                    .map(|v| v.to_vec())
                    .collect::<Vec<Vec<u8>>>()
                    .concat()
                    .as_slice(),
                [b'$'].as_slice(),
            ]
            .concat(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Parse(String),
    Other(String),
}

impl TryFrom<&[u8]> for Value {
    type Error = Error;

    fn try_from(v: &[u8]) -> Result<Self, Error> {
        value(v)
            .finish()
            .map(|(_, res)| res)
            .map_err(|e| Error::Parse(format!("{:?}", e)))
    }
}

impl FromStr for Value {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        value(s.as_bytes())
            .finish()
            .map(|(_, res)| res)
            .map_err(|e| Error::Parse(format!("{:?}", e)))
    }
}

fn value(input: &[u8]) -> IResult<&[u8], Value> {
    context(
        "value",
        alt((
            boolean_value,
            float_value,
            double_value,
            integer_value,
            binary_value,
            string_value,
            symbol_value,
            dictionary_value,
            sequence_value,
            record_value,
            set_value,
        )),
    )(input)
}

fn boolean_value(input: &[u8]) -> IResult<&[u8], Value> {
    context("boolean", alt((tag("t"), tag("f"))))(input).map(|(next_input, res)| {
        (
            next_input,
            match res {
                b"t" => Value::Boolean(true),
                b"f" => Value::Boolean(false),
                _ => unreachable!("parser"),
            },
        )
    })
}

fn float_value(input: &[u8]) -> IResult<&[u8], Value> {
    context("float", preceded(tag("F"), take(4u8)))(input).map(|(next_input, res)| {
        (
            next_input,
            Value::Float(f32::from_be_bytes(res.try_into().unwrap())),
        )
    })
}

fn double_value(input: &[u8]) -> IResult<&[u8], Value> {
    context("double", preceded(tag("D"), take(8u8)))(input).map(|(next_input, res)| {
        (
            next_input,
            Value::Double(f64::from_be_bytes(res.try_into().unwrap())),
        )
    })
}

fn integer_value(input: &[u8]) -> IResult<&[u8], Value> {
    context("integer", pair(digit1, alt((tag("+"), tag("-")))))(input).map(|(next_input, res)| {
        let (num_str, sign_str) = res;
        let sign = match sign_str {
            b"+" => Sign::Plus,
            b"-" => Sign::Minus,
            _ => unreachable!(),
        };
        (
            next_input,
            Value::Integer(
                BigInt::from_radix_be(
                    sign,
                    num_str
                        .iter()
                        .map(|d| d - 0x30)
                        .collect::<Vec<u8>>()
                        .as_slice(),
                    10,
                )
                .unwrap(),
            ),
        )
    })
}

fn binary_value(input: &[u8]) -> IResult<&[u8], Value> {
    context(
        "binary",
        length_count(
            terminated(digit1, tag(":"))
                .map(|res| u32::from_str(String::from_utf8_lossy(res).as_ref()).unwrap()),
            take(1u8),
        ),
    )(input)
    .map(|(next_input, res)| {
        (
            next_input,
            Value::Binary(res.iter().map(|b| b[0]).collect()),
        )
    })
}

fn string_value(input: &[u8]) -> IResult<&[u8], Value> {
    context(
        "string",
        length_count(
            terminated(digit1, tag("\""))
                .map(|res| u32::from_str(String::from_utf8_lossy(res).as_ref()).unwrap()),
            take(1u8),
        ),
    )(input)
    .map(|(next_input, res)| {
        (
            next_input,
            Value::String(
                String::from_utf8_lossy(res.iter().map(|b| b[0]).collect::<Vec<u8>>().as_slice())
                    .into_owned(),
            ),
        )
    })
}

fn symbol_value(input: &[u8]) -> IResult<&[u8], Value> {
    context(
        "symbol",
        length_count(
            terminated(digit1, tag("\'"))
                .map(|res| u32::from_str(String::from_utf8_lossy(res).as_ref()).unwrap()),
            take(1u8),
        ),
    )(input)
    .map(|(next_input, res)| {
        (
            next_input,
            Value::Symbol(
                String::from_utf8_lossy(res.iter().map(|b| b[0]).collect::<Vec<u8>>().as_slice())
                    .into_owned(),
            ),
        )
    })
}

fn sequence_value(input: &[u8]) -> IResult<&[u8], Value> {
    context("sequence", preceded(tag("["), many_till(value, tag("]"))))(input)
        .map(|(next_input, res)| (next_input, Value::Sequence(res.0)))
}

fn dictionary_value(input: &[u8]) -> IResult<&[u8], Value> {
    context(
        "dictionary",
        preceded(tag("{"), many_till(pair(value, value), tag("}"))),
    )(input)
    .map(|(next_input, mut res)| {
        res.0.sort();
        (next_input, Value::Dictionary(res.0))
    })
}

fn record_value(input: &[u8]) -> IResult<&[u8], Value> {
    context(
        "sequence",
        preceded(tag("<"), pair(value, many_till(value, tag(">")))),
    )(input)
    .map(|(next_input, res)| {
        (
            next_input,
            Value::Record {
                label: Box::new(res.0),
                fields: res.1 .0,
            },
        )
    })
}

fn set_value(input: &[u8]) -> IResult<&[u8], Value> {
    context("sequence", preceded(tag("#"), many_till(value, tag("$"))))(input).map(
        |(next_input, mut res)| {
            res.0.sort();
            (next_input, Value::Set(res.0))
        },
    )
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        return self.cmp(other).is_eq();
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_vec().hash(state);
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cmp(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_from_slice() {
        assert_eq!(b"t".as_slice().try_into(), Ok(Value::Boolean(true)),);
        assert_eq!(b"f".as_slice().try_into(), Ok(Value::Boolean(false)),);
        assert_eq!(
            b"F\x3d\xcc\xcc\xcd".as_slice().try_into(),
            Ok(Value::Float(0.1)),
        );
        assert_eq!(
            b"D\x3f\xb9\x99\x99\x99\x99\x99\x9a".as_slice().try_into(),
            Ok(Value::Double(0.1)),
        );
    }

    #[test]
    fn from_str() {
        assert_eq!(Value::from_str("t"), Ok(Value::Boolean(true)),);
        assert_eq!(Value::from_str("f"), Ok(Value::Boolean(false)),);
        assert_eq!(Value::from_str("42+"), Ok(Value::Integer(42.into())),);
        assert_eq!(Value::from_str("42-"), Ok(Value::Integer((-42).into())),);
        assert_eq!(
            Value::from_str("5:hello"),
            Ok(Value::Binary(b"hello".to_vec()))
        );
        assert_eq!(
            Value::from_str("3\"foo"),
            Ok(Value::String("foo".to_string()))
        );
        assert_eq!(
            Value::from_str("3'foo"),
            Ok(Value::Symbol("foo".to_string()))
        );
        assert_eq!(
            Value::from_str("[1+2+3+]"),
            Ok(Value::Sequence(vec![
                Value::Integer(1.into()),
                Value::Integer(2.into()),
                Value::Integer(3.into()),
            ]))
        );
        assert_eq!(
            Value::from_str("{3\"goo4\"muck3\"foo3\"bar}"),
            Ok(Value::Dictionary(vec![
                (
                    Value::String("foo".to_string()),
                    Value::String("bar".to_string())
                ),
                (
                    Value::String("goo".to_string()),
                    Value::String("muck".to_string())
                )
            ]))
        );
        assert_eq!(
            Value::from_str("<6:person5:Alice30+t>"),
            Ok(Value::Record {
                label: Box::new(Value::Binary(b"person".to_vec())),
                fields: vec![
                    Value::Binary(b"Alice".to_vec()),
                    Value::Integer(30.into()),
                    Value::Boolean(true),
                ]
            })
        );
        assert_eq!(
            Value::from_str("#3\"foo3\"bar$"),
            Ok(Value::Set(vec![
                Value::String("bar".to_string()),
                Value::String("foo".to_string())
            ]))
        );
    }

    #[test]
    fn round_trip_from_str_to_vec() {
        for s in [
            "t",
            "f",
            "10+",
            "10-",
            "5:hello",
            "3\"foo",
            "4'none",
            "[1+2+3+]",
            "{3\"foo3\"bar3\"goo4\"muck}",
            "<6:person5:Alice30+t>",
            "#3\"bar3\"foo$",
        ] {
            assert_eq!(
                Value::from_str(s).unwrap().to_vec(),
                s.as_bytes().to_vec(),
                "round trip value: {}",
                s
            );
        }
    }
}
