extern crate num;
use num::rational::{Ratio, Rational64};
use std::fmt;
use std::iter::{Product, Sum};
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Copy, Clone, PartialOrd, Ord, Eq)]
pub enum Number {
    /// 有理数
    Rat(Rational64),
    /// 整数
    Int(i64),
}
impl Number {
    /// 正規化
    fn normal(self) -> Self {
        match self {
            Self::Int(_) => self,
            Self::Rat(r) => {
                if r.is_integer() {
                    Number::Int(r.to_integer() as i64)
                } else {
                    self
                }
            }
        }
    }

    /// 有理数にする
    fn to_rat(self) -> Rational64 {
        match self {
            Self::Int(n) => Ratio::from_integer(n),
            Self::Rat(r) => r,
        }
    }

    pub fn is_zero(self) -> bool {
        match self {
            Self::Int(n) => n == 0,
            Self::Rat(r) => *r.numer() == 0,
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let n = self.normal();
        match n {
            Self::Rat(r) => write!(f, "{}", r),
            Self::Int(i) => write!(f, "{}", i),
        }
    }
}

macro_rules! expr {
    ($e:expr) => {
        $e
    };
}
macro_rules! calc {
    ($s:expr, $op:tt, $o:expr) => {{
        let lhs = $s.to_rat();
        let rhs = $o.to_rat();
        let result = expr!(lhs $op rhs);
        Self::Rat(result).normal()
    }};
}

impl Add for Number {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        calc!(self, +, other)
    }
}
impl Sub for Number {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        calc!(self, -, other)
    }
}
impl Mul for Number {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        calc!(self, *, other)
    }
}
impl Div for Number {
    type Output = Self;
    fn div(self, other: Self) -> Self {
        calc!(self, /, other)
    }
}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        let lhs = self.to_rat();
        let rhs = other.to_rat();
        lhs == rhs
    }
}

// into_iter().sum()
impl Sum for Number {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        let mut s = Self::Int(0);
        for n in iter {
            s = s + n;
        }
        s
    }
}

// iter().sum()
impl<'a> Sum<&'a Number> for Number {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        let mut s = Self::Int(0);
        for n in iter {
            s = s + *n;
        }
        s
    }
}
impl Product for Number {
    fn product<I>(iter: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        let mut s = Self::Int(1);
        for n in iter {
            s = s * n;
        }
        s
    }
}

impl<'a> Product<&'a Number> for Number {
    fn product<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        let mut s = Self::Int(1);
        for n in iter {
            s = s * *n;
        }
        s
    }
}

// 数をNumberに変更．コメントアウトしたところなどのチェック中
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    /// BOOLEAN
    Bool(bool),
    /// 数
    Num(Number),
    /// nil
    Nil,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Bool(b) => {
                if *b {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
            Self::Num(n) => write!(f, "{}", n),
            Self::Nil => write!(f, "()"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_number_add() {
        let tests = vec![
            ((Number::Int(1), Number::Int(2)), Number::Int(3)),
            (
                (Number::Rat(Ratio::new(1, 3)), Number::Rat(Ratio::new(1, 3))),
                Number::Rat(Ratio::new(2, 3)),
            ),
            (
                (Number::Rat(Ratio::new(1, 3)), Number::Int(1)),
                Number::Rat(Ratio::new(4, 3)),
            ),
            (
                (Number::Rat(Ratio::new(2, 5)), Number::Rat(Ratio::new(3, 5))),
                Number::Int(1),
            ),
        ];
        for ((l, r), expected) in tests.into_iter() {
            assert_eq!(expected, l + r)
        }
    }

    #[test]
    fn test_number_sub() {
        let tests = vec![
            ((Number::Int(1), Number::Int(2)), Number::Int(-1)),
            (
                (Number::Rat(Ratio::new(2, 3)), Number::Rat(Ratio::new(1, 3))),
                Number::Rat(Ratio::new(1, 3)),
            ),
            (
                (Number::Rat(Ratio::new(1, 3)), Number::Int(1)),
                Number::Rat(Ratio::new(-2, 3)),
            ),
            (
                (
                    Number::Rat(Ratio::new(10, 5)),
                    Number::Rat(Ratio::new(5, 5)),
                ),
                Number::Int(1),
            ),
        ];
        for ((l, r), expected) in tests.into_iter() {
            assert_eq!(expected, l - r)
        }
    }

    #[test]
    fn test_number_mul() {
        let tests = vec![
            ((Number::Int(1), Number::Int(1)), Number::Int(1)),
            ((Number::Int(2), Number::Int(3)), Number::Int(6)),
            ((Number::Int(1), Number::Int(0)), Number::Int(0)),
            (
                (Number::Rat(Ratio::new(1, 3)), Number::Rat(Ratio::new(2, 3))),
                Number::Rat(Ratio::new(2, 9)),
            ),
            (
                (Number::Rat(Ratio::new(1, 3)), Number::Int(0)),
                Number::Int(0),
            ),
            (
                (Number::Rat(Ratio::new(2, 3)), Number::Rat(Ratio::new(9, 3))),
                Number::Int(2),
            ),
        ];
        for ((l, r), expected) in tests.into_iter() {
            assert_eq!(expected, l * r)
        }
    }

    #[test]
    fn test_number_div() {
        let tests = vec![
            ((Number::Int(1), Number::Int(1)), Number::Int(1)),
            ((Number::Int(6), Number::Int(3)), Number::Int(2)),
            (
                (Number::Rat(Ratio::new(1, 3)), Number::Rat(Ratio::new(2, 3))),
                Number::Rat(Ratio::new(1, 2)),
            ),
            (
                (Number::Rat(Ratio::new(4, 3)), Number::Rat(Ratio::new(2, 3))),
                Number::Int(2),
            ),
        ];
        for ((l, r), expected) in tests.into_iter() {
            assert_eq!(expected, l / r)
        }
    }

    #[test]
    fn test_number_sum() {
        let tests = vec![
            (
                vec![Number::Int(1), Number::Int(2), Number::Int(3)],
                Number::Int(6),
            ),
            (
                vec![Number::Int(2), Number::Int(3), Number::Int(-2)],
                Number::Int(3),
            ),
        ];
        for (v, expected) in tests.into_iter() {
            assert_eq!(expected, v.iter().sum::<Number>());
            assert_eq!(expected, v.into_iter().sum::<Number>())
        }
    }

    #[test]
    fn test_number_product() {
        let tests = vec![
            (
                vec![Number::Int(2), Number::Int(3), Number::Int(4)],
                Number::Int(24),
            ),
            (
                vec![Number::Int(2), Number::Int(3), Number::Int(-2)],
                Number::Int(-12),
            ),
            (
                vec![Number::Int(2), Number::Int(3), Number::Int(0)],
                Number::Int(0),
            ),
        ];
        for (v, expected) in tests.into_iter() {
            assert_eq!(expected, v.iter().product::<Number>());
            assert_eq!(expected, v.into_iter().product::<Number>())
        }
    }
}
