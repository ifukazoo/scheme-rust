extern crate num;
use num::rational::Rational64;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    /// 有理数
    Rat(Rational64),
    /// BOOLEAN
    Bool(bool),
    /// nil
    Nil,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Rat(r) => {
                if *r.denom() == 1 {
                    write!(f, "{}", r.numer())
                } else {
                    write!(f, "{}/{}", r.numer(), r.denom())
                }
            }
            Self::Bool(b) => {
                if *b {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
            Self::Nil => write!(f, "()"),
        }
    }
}
