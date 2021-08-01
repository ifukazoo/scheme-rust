extern crate num;
use crate::env;
use crate::env::RefEnv;
use crate::parser::Unit;
use num::rational::{Ratio, Rational64};
use num::Zero;
use std::fmt;
use std::iter::{Product, Sum};
use std::ops::{Add, Div, Mul, Sub};

/// scheme評価結果値
#[derive(Debug, Clone)]
pub enum Object {
    /// BOOLEAN
    Bool(bool),
    /// 数
    Num(Number),
    /// ペア
    Pair(Box<Object>, Box<Object>),
    /// クロージャー. param, block, env
    Procedure(Vec<Unit>, Option<Vec<Unit>>, RefEnv),
    /// サブルーチン
    Subr(&'static str),
    /// nil
    Nil,
    /// 未定義．
    Undef,
}

// ペアを作成する
pub fn cons_pair(lhs: Object, rhs: Object) -> Object {
    Object::Pair(Box::new(lhs), Box::new(rhs))
}
// リスト(Nilを終端に持つペアの再帰構造)を作成する
pub fn build_list(args: Vec<Object>) -> Object {
    // [1,2,3] => [3,nil] => [2,[3,nil]] => [1,[2,[3,nil]]]

    let mut o = Object::Nil;
    for arg in args.iter().rev() {
        o = cons_pair(arg.clone(), o);
    }
    o
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", to_string_bool(*b)),
            Self::Num(n) => write!(f, "{}", n),
            Self::Nil => write!(f, "()"),
            Self::Undef => write!(f, "{}", to_string_undef()),
            Self::Pair(first, second) => {
                let first = first.clone();
                let second = second.clone();
                write!(f, "{}", to_string_pair(*first, *second))
            }
            Self::Procedure(params, _, _) => write!(f, "{}", to_string_closure(&params)),
            Self::Subr(s) => write!(f, "#<subr ({} :rest args)>", s),
        }
    }
}
impl PartialEq for Object {
    fn eq(&self, rhs: &Object) -> bool {
        use Object::*;
        match self {
            Undef => match rhs {
                Undef => true,
                _ => false,
            },
            Nil => match rhs {
                Nil => true,
                _ => false,
            },
            Bool(l) => match rhs {
                Bool(r) => l == r,
                _ => false,
            },
            Num(l) => match rhs {
                Num(r) => l == r,
                _ => false,
            },
            Subr(l) => match rhs {
                Subr(r) => l == r,
                _ => false,
            },
            Procedure(lp, lb, le) => match rhs {
                Procedure(rp, rb, re) => {
                    let b1 = lp == rp;
                    let b2 = lb == rb;
                    let b3 = env::equals(&le, &re);
                    b1 && b2 && b3
                }
                _ => false,
            },
            Pair(lf, ls) => match rhs {
                Pair(rf, rs) => {
                    let b1 = lf == rf;
                    let b2 = ls == rs;
                    b1 && b2
                }
                _ => false,
            },
        }
    }
}

fn to_string_bool(b: bool) -> String {
    if b {
        "#t".to_string()
    } else {
        "#f".to_string()
    }
}
fn to_string_undef() -> String {
    "<#undef>".to_string()
}
fn to_string_pair(first: Object, second: Object) -> String {
    let mut buf = String::new();
    to_string_pair_1st(first, second, &mut buf);
    buf
}
fn to_string_pair_1st(first: Object, second: Object, collecting: &mut String) {
    // ペアの先頭を出力
    if collecting.is_empty() {
        collecting.push_str(&format!("({}", first));
    } else {
        collecting.push_str(&format!(" {}", first));
    }
    to_string_pair_2nd(second, collecting);
}
fn to_string_pair_2nd(second: Object, collecting: &mut String) {
    match second {
        // ペア終端がnilの場合は値分は何も出力せず，終端の`)`だけ出力
        Object::Nil => collecting.push(')'),
        Object::Pair(first, second) => {
            to_string_pair_1st(*first, *second, collecting);
        }
        etc => {
            collecting.push_str(&format!(" . {}", etc));
            collecting.push(')');
        }
    }
}
fn to_string_closure(params: &[Unit]) -> String {
    let mut ps = String::new();
    for p in params {
        ps.push_str(&format!(" {}", &p));
    }
    format!("#<closure (#f{})>", ps)
}

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
}

impl Zero for Number {
    fn zero() -> Self {
        Self::Int(0)
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Int(n) => *n == 0,
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
            assert_eq!(expected, v.into_iter().product::<Number>());
        }
    }

    #[test]
    fn test_number_string() {
        use super::super::env::*;
        use super::super::eval::*;
        use super::super::lexer::*;
        use super::super::parser::*;
        use std::collections::HashMap;

        let tests = vec![
            ("(cons 1 2)", "(1 . 2)"),
            ("(cons 1 ())", "(1)"),
            ("(cons 1 (cons 2 ()))", "(1 2)"),
            ("(cons (define a) ())", "(<#undef>)"),
            ("(cons (> 2 1) (> 1 2))", "(#t . #f)"),
            ("(cons (cons (> 2 1) (> 1 2)) ())", "((#t . #f))"),
        ];

        for (input, expected) in tests.into_iter() {
            let token = lex(input).unwrap();
            let elements = parse_program(token).unwrap();
            let env = new_env(HashMap::new());
            let obj = eval(elements, &env).unwrap();
            assert_eq!(expected, format!("{}", obj))
        }
    }

    #[test]
    fn test_partialeq_bool() {
        use Number::*;
        use Object::*;
        let tests = vec![
            //
            ((Bool(true), Bool(true)), true),
            ((Bool(true), Bool(false)), false),
            //
            ((Bool(true), Num(Int(1))), false),
        ];

        for ((a, b), expected) in tests.into_iter() {
            assert_eq!(expected, a == b);
        }
    }

    #[test]
    fn test_partialeq_num() {
        use Number::*;
        use Object::*;
        let tests = vec![
            //
            ((Bool(true), Bool(true)), true),
            ((Bool(true), Bool(false)), false),
            ((Bool(true), Num(Int(1))), false),
            //
            ((Num(Int(1)), Num(Int(1))), true),
            ((Num(Int(0)), Num(Int(1))), false),
            ((Num(Int(1)), Num(Rat(Ratio::new(2, 2)))), true),
            ((Num(Int(1)), Num(Rat(Ratio::new(4, 2)))), false),
            ((Num(Int(1)), Bool(true)), false),
            //
        ];

        for ((a, b), expected) in tests.into_iter() {
            assert_eq!(expected, a == b);
        }
    }
    #[test]
    fn test_partialeq_pair() {
        use Number::*;
        use Object::*;
        let tests = vec![
            // 同一種 true, false
            (
                (
                    cons_pair(Bool(true), Num(Int(1))),
                    cons_pair(Bool(true), Num(Int(1))),
                ),
                true,
            ),
            (
                (
                    build_list(vec![Bool(true), Bool(false), Num(Int(1))]),
                    build_list(vec![Bool(true), Bool(false), Num(Int(1))]),
                ),
                true,
            ),
            (
                (
                    cons_pair(Bool(true), Num(Int(1))),
                    cons_pair(Bool(true), Num(Int(2))),
                ),
                false,
            ),
            (
                (
                    cons_pair(Bool(true), Num(Int(1))),
                    build_list(vec![Bool(true), Bool(false), Num(Int(1))]),
                ),
                false,
            ),
            // 別種
            ((cons_pair(Bool(true), Num(Int(1))), Bool(true)), false),
        ];

        for ((a, b), expected) in tests.into_iter() {
            assert_eq!(expected, a == b);
        }
    }
    #[test]
    fn test_partialeq_procedure() {
        use crate::parser::Atom;
        use crate::parser::Atom::*;
        use crate::parser::Unit::*;
        use std::collections::HashMap;
        use Object::*;
        let env_a = env::new_env(HashMap::new());
        let env_b = env::new_env(HashMap::new());
        let param_a = vec![Bare(Ident("a".to_string()))];
        let param_b = vec![Bare(Ident("a".to_string()))];
        let param_c = vec![Bare(Ident("c".to_string()))];

        let tests = vec![
            // 同一種 true, false
            // true
            (
                (
                    Procedure(param_a.clone(), None, env_a.clone()),
                    Procedure(param_a.clone(), None, env_a.clone()),
                ),
                true,
            ),
            (
                (
                    Procedure(param_a.clone(), None, env_a.clone()),
                    Procedure(param_b.clone(), None, env_a.clone()),
                ),
                true,
            ),
            (
                (
                    Procedure(
                        param_a.clone(),
                        Some(vec![Bare(Atom::Num(1))]),
                        env_a.clone(),
                    ),
                    Procedure(
                        param_a.clone(),
                        Some(vec![Bare(Atom::Num(1))]),
                        env_a.clone(),
                    ),
                ),
                true,
            ),
            // false
            (
                (
                    Procedure(param_a.clone(), None, env_a.clone()),
                    Procedure(param_c.clone(), None, env_a.clone()),
                ),
                false,
            ),
            (
                (
                    Procedure(param_a.clone(), None, env_a.clone()),
                    Procedure(
                        param_b.clone(),
                        Some(vec![Bare(Atom::Num(1))]),
                        env_a.clone(),
                    ),
                ),
                false,
            ),
            (
                (
                    Procedure(
                        param_a.clone(),
                        Some(vec![Bare(Atom::Num(1))]),
                        env_a.clone(),
                    ),
                    Procedure(
                        param_a.clone(),
                        Some(vec![Bare(Atom::Num(1))]),
                        env_b.clone(),
                    ),
                ),
                false,
            ),
            (
                (
                    Procedure(param_a.clone(), None, env_a.clone()),
                    Object::Bool(true),
                ),
                false,
            ),
        ];

        for ((a, b), expected) in tests.into_iter() {
            assert_eq!(expected, a == b);
        }
    }
    #[test]
    fn test_partialeq_subroutine() {
        use Number::*;
        use Object::*;
        let tests = vec![
            // 同一種 true, false
            ((Subr("car"), Subr("car")), true),
            ((Subr("car"), Subr("cdr")), false),
            // 別種
            ((Subr("car"), Num(Int(1))), false),
        ];

        for ((a, b), expected) in tests.into_iter() {
            assert_eq!(expected, a == b);
        }
    }
    #[test]
    fn test_partialeq_nil_undef() {
        use Object::*;
        let tests = vec![
            ((Nil, Nil), true),
            ((Undef, Undef), true),
            ((Nil, Undef), false),
        ];

        for ((a, b), expected) in tests.into_iter() {
            assert_eq!(expected, a == b);
        }
    }
}
