use crate::token::Token;
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

    // その他の変数
    Ident(String),
}

// 構文解析エラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    EmptyProgram,
    UnclosedParen,
    IllegalSyntax(Token),
}

pub fn parse_program(tokens: Vec<Token>) -> Result<Unit, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    if tokens.len() == 0 {
        Err(ParseError::EmptyProgram)
    } else if tokens.len() == 1 {
        let a = parse_atom(&mut tokens)?;
        Ok(Unit::Bare(a))
    } else {
        match parse_array(&mut tokens) {
            Ok(a) => Ok(Unit::Paren(a)),
            Err(e) => Err(e),
        }
    }
}

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
        Token::EQUAL => Ok(Atom::App("eq?")),
        Token::NOT => Ok(Atom::App("not")),
        Token::IF => Ok(Atom::App("if")),
        Token::COND => Ok(Atom::App("cond")),
        Token::ELSE => Ok(Atom::App("else")),
        Token::LET => Ok(Atom::App("let")),
        Token::VAR(s) => Ok(Atom::Ident(s)),
        _ => Err(ParseError::IllegalSyntax(t)),
    }
}
fn parse_array<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Vec<Unit>, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    // `(`の刈り取り
    tokens.next().unwrap();

    let mut arr = vec![];
    while let Some(e) = tokens.peek() {
        match e {
            Token::RPAREN => break,
            Token::LPAREN => {
                let inner = parse_array(tokens)?;
                arr.push(Unit::Paren(inner));
            }
            _ => {
                let a = parse_atom(tokens)?;
                arr.push(Unit::Bare(a));
            }
        }
    }

    if tokens.peek().is_none() {
        Err(ParseError::UnclosedParen)
    } else {
        // `)`の刈り取り
        tokens.next().unwrap();
        Ok(arr)
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
        ];

        for (input, expected) in tests.into_iter() {
            match parse_program(lexer::lex(input).unwrap()) {
                Ok(_) => panic!("expected err. but ok."),
                Err(e) => assert_eq!(expected, e),
            }
        }
    }
}
