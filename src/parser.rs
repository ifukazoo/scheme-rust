use crate::token::Token;
use std::fmt;
use std::iter::Peekable;

/// プログラムの要素
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Unit {
    // ()なし
    Bare(Atom),
    // ()あり
    Paren(Vec<Unit>),
}

/// Atom 分割できないもの
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Atom {
    /// 演算子．+, -, lambda, eq, not, ...
    App(&'static str),
    /// 数. 0, 1, 2,
    Num(i64),
    /// 真偽
    Bool(bool),
    // Str(String)
    // 文字列. 'hello'
    /// 識別子
    Ident(String),
}
impl fmt::Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Bare(a) => match a {
                Atom::App(app) => write!(f, "{}", app),
                Atom::Num(n) => write!(f, "{}", n),
                Atom::Bool(b) => write!(f, "{}", b),
                Atom::Ident(i) => write!(f, "{}", i),
            },
            Self::Paren(units) => {
                let mut s = String::from("(");
                let mut sep = String::from("");
                for unit in units {
                    s.push_str(&format!("{}{}", sep, unit));
                    sep = " ".to_string();
                }
                s.push(')');
                write!(f, "{}", s)
            }
        }
    }
}

// 構文解析エラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    /// 空のプログラム
    EmptyProgram,
    /// 閉じてないかっこ
    UnclosedParen,
    /// 余分なトークン
    ExtraToken(Token),
    /// 不正な構文
    IllegalSyntax(Token),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::EmptyProgram => write!(f, "empty program."),
            Self::UnclosedParen => write!(f, "unclosed paren."),
            Self::ExtraToken(t) => write!(f, "extra token:[{:?}].", t),
            Self::IllegalSyntax(t) => write!(f, "iextra token:[{:?}].", t),
        }
    }
}

/// 構文解析
pub fn parse_program(tokens: Vec<Token>) -> Result<Unit, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    if tokens.len() == 0 {
        Err(ParseError::EmptyProgram)
    } else if tokens.len() == 1 {
        let a = parse_atom(&mut tokens)?;
        Ok(Unit::Bare(a))
    } else {
        let units = parse_array(&mut tokens)?;
        if tokens.peek().is_none() {
            Ok(Unit::Paren(units))
        } else {
            Err(ParseError::ExtraToken(tokens.next().unwrap()))
        }
    }
}

// 単体要素の解析
fn parse_atom<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Atom, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let t = tokens.next().unwrap();
    match t {
        Token::INT(n) => Ok(Atom::Num(n)),
        Token::TRUE => Ok(Atom::Bool(true)),
        Token::FALSE => Ok(Atom::Bool(false)),
        Token::PLUS => Ok(Atom::App("+")),
        Token::MINUS => Ok(Atom::App("-")),
        Token::ASTER => Ok(Atom::App("*")),
        Token::SLASH => Ok(Atom::App("/")),
        Token::GT => Ok(Atom::App(">")),
        Token::LT => Ok(Atom::App("<")),
        Token::BEGIN => Ok(Atom::App("begin")),
        Token::DEFINE => Ok(Atom::App("define")),
        Token::SET => Ok(Atom::App("set")),
        Token::CONS => Ok(Atom::App("cons")),
        Token::CAR => Ok(Atom::App("car")),
        Token::CDR => Ok(Atom::App("cdr")),
        Token::LIST => Ok(Atom::App("list")),
        Token::EQUAL => Ok(Atom::App("equal")),
        Token::NOT => Ok(Atom::App("not")),
        Token::ISZERO => Ok(Atom::App("zero")),
        Token::IF => Ok(Atom::App("if")),
        Token::COND => Ok(Atom::App("cond")),
        Token::ELSE => Ok(Atom::App("else")),
        Token::LET => Ok(Atom::App("let")),
        Token::LETA => Ok(Atom::App("leta")),
        Token::LETREC => Ok(Atom::App("letrec")),
        Token::LAMBDA => Ok(Atom::App("lambda")),
        Token::VAR(s) => Ok(Atom::Ident(s)),
        _ => Err(ParseError::IllegalSyntax(t)),
    }
}

// かっこに囲まれた複数要素の解析
fn parse_array<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Vec<Unit>, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    // `(`の刈り取り
    tokens.next().unwrap();

    let mut units = vec![];
    while let Some(e) = tokens.peek() {
        match e {
            Token::RPAREN => break,
            Token::LPAREN => {
                let inner_units = parse_array(tokens)?;
                units.push(Unit::Paren(inner_units));
            }
            _ => {
                let atom = parse_atom(tokens)?;
                units.push(Unit::Bare(atom));
            }
        }
    }

    // while を抜ける条件
    //   右かっこでbreak
    //   or
    //   peek() == NONE

    if tokens.peek().is_none() {
        Err(ParseError::UnclosedParen)
    } else {
        // `)`の刈り取り
        tokens.next().unwrap();
        Ok(units)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer;

    #[test]
    fn test_parser() {
        let tests = vec![
            ("1", Unit::Bare(Atom::Num(1))),
            ("()", Unit::Paren(vec![])),
            (
                "(+ 1 2 )",
                Unit::Paren(vec![
                    Unit::Bare(Atom::App("+")),
                    Unit::Bare(Atom::Num(1)),
                    Unit::Bare(Atom::Num(2)),
                ]),
            ),
            (
                "(+ 1 (+ 2 3 ))",
                Unit::Paren(vec![
                    Unit::Bare(Atom::App("+")),
                    Unit::Bare(Atom::Num(1)),
                    Unit::Paren(vec![
                        Unit::Bare(Atom::App("+")),
                        Unit::Bare(Atom::Num(2)),
                        Unit::Bare(Atom::Num(3)),
                    ]),
                ]),
            ),
            ("+", Unit::Bare(Atom::App("+"))),
            (
                "(begin (+ 2 1) (+ 2 3))",
                Unit::Paren(vec![
                    Unit::Bare(Atom::App("begin")),
                    Unit::Paren(vec![
                        Unit::Bare(Atom::App("+")),
                        Unit::Bare(Atom::Num(2)),
                        Unit::Bare(Atom::Num(1)),
                    ]),
                    Unit::Paren(vec![
                        Unit::Bare(Atom::App("+")),
                        Unit::Bare(Atom::Num(2)),
                        Unit::Bare(Atom::Num(3)),
                    ]),
                ]),
            ),
            (
                r#"
                (begin
                    (define abc)
                )"#,
                Unit::Paren(vec![
                    Unit::Bare(Atom::App("begin")),
                    Unit::Paren(vec![
                        Unit::Bare(Atom::App("define")),
                        Unit::Bare(Atom::Ident("abc".to_string())),
                    ]),
                ]),
            ),
        ];
        for (input, expected) in tests.into_iter() {
            let program = parse_program(lexer::lex(input).unwrap()).unwrap();
            assert_eq!(expected, program)
        }
    }
    #[test]
    fn test_parser_ng() {
        let tests = vec![
            ("( + ", ParseError::UnclosedParen),
            ("  ", ParseError::EmptyProgram),
            ("( ", ParseError::IllegalSyntax(Token::LPAREN)),
            (" )", ParseError::IllegalSyntax(Token::RPAREN)),
            ("())", ParseError::ExtraToken(Token::RPAREN)),
        ];

        for (input, expected) in tests.into_iter() {
            match parse_program(lexer::lex(input).unwrap()) {
                Ok(_) => panic!("expected err. but ok."),
                Err(e) => assert_eq!(expected, e),
            }
        }
    }
    #[test]
    fn test_unit_disp() {
        let tests = vec![
            (Unit::Bare(Atom::App("a")), "a"),
            (Unit::Bare(Atom::Bool(true)), "true"),
            (
                Unit::Paren(vec![
                    Unit::Bare(Atom::App("list")),
                    Unit::Bare(Atom::App("a")),
                    Unit::Bare(Atom::Bool(true)),
                ]),
                "(list a true)",
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(expected, format!("{}", input))
        }
    }
}
