use crate::eval::EvalError;
use crate::lexer::LexError;
use crate::parser::ParseError;
use std::fmt;
use std::io;

/// Scheme-Rustエラー
#[derive(Debug)]
pub enum SRError {
    // 評価時のエラー
    Eval(EvalError),
    // 構文解析時のエラー
    Parser(ParseError),
    // 字句解析時のエラー
    Lexer(LexError),
    // IOエラー
    Io(io::Error),
}

impl From<EvalError> for SRError {
    fn from(e: EvalError) -> Self {
        Self::Eval(e)
    }
}
impl From<ParseError> for SRError {
    fn from(e: ParseError) -> Self {
        Self::Parser(e)
    }
}
impl From<LexError> for SRError {
    fn from(e: LexError) -> Self {
        Self::Lexer(e)
    }
}
impl From<io::Error> for SRError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl fmt::Display for SRError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Eval(e) => write!(f, "eval error:{}", e),
            Self::Parser(e) => write!(f, "parser error:{}", e),
            _ => todo!(),
            // Self::Io(e) => write!(f, "IO error:{}", e),
        }
    }
}
