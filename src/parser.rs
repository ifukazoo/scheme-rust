use crate::token::Token;
use std::iter::Peekable;

/// プログラムの要素
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Element {
    A(Atom),
    V(Vec<Element>),
}

/// Atom 分割できないもの
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Atom {
    /// 演算子．+, -, lambda, eq, not, ...
    Ope(&'static str),
    /// 数. 0, 1, 2,
    Num(i64),
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

pub fn parse_program(tokens: Vec<Token>) -> Result<Element, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    if tokens.len() == 0 {
        Err(ParseError::EmptyProgram)
    } else if tokens.len() == 1 {
        let a = parse_atom(&mut tokens)?;
        Ok(Element::A(a))
    } else {
        match parse_array(&mut tokens) {
            Ok(a) => Ok(Element::V(a)),
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
        Token::PLUS => Ok(Atom::Ope("+")),
        Token::MINUS => Ok(Atom::Ope("-")),
        Token::ASTER => Ok(Atom::Ope("*")),
        Token::SLASH => Ok(Atom::Ope("/")),
        Token::GT => Ok(Atom::Ope(">")),
        Token::LT => Ok(Atom::Ope("<")),
        Token::BEGIN => Ok(Atom::Ope("begin")),
        Token::DEFINE => Ok(Atom::Ope("define")),
        Token::SET => Ok(Atom::Ope("set")),
        Token::CONS => Ok(Atom::Ope("cons")),
        Token::VAR(s) => Ok(Atom::Ident(s)),
        _ => Err(ParseError::IllegalSyntax(t)),
    }
}
fn parse_array<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Vec<Element>, ParseError>
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
                arr.push(Element::V(inner));
            }
            _ => {
                let a = parse_atom(tokens)?;
                arr.push(Element::A(a));
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
            ("1", Element::A(Atom::Num(1))),
            ("()", Element::V(vec![])),
            (
                "(+ 1 2 )",
                Element::V(vec![
                    Element::A(Atom::Ope("+")),
                    Element::A(Atom::Num(1)),
                    Element::A(Atom::Num(2)),
                ]),
            ),
            (
                "(+ 1 (+ 2 3 ))",
                Element::V(vec![
                    Element::A(Atom::Ope("+")),
                    Element::A(Atom::Num(1)),
                    Element::V(vec![
                        Element::A(Atom::Ope("+")),
                        Element::A(Atom::Num(2)),
                        Element::A(Atom::Num(3)),
                    ]),
                ]),
            ),
            ("+", Element::A(Atom::Ope("+"))),
            (
                "(begin (+ 2 1) (+ 2 3))",
                Element::V(vec![
                    Element::A(Atom::Ope("begin")),
                    Element::V(vec![
                        Element::A(Atom::Ope("+")),
                        Element::A(Atom::Num(2)),
                        Element::A(Atom::Num(1)),
                    ]),
                    Element::V(vec![
                        Element::A(Atom::Ope("+")),
                        Element::A(Atom::Num(2)),
                        Element::A(Atom::Num(3)),
                    ]),
                ]),
            ),
            (
                r#"
                (begin
                    (define abc)
                )"#,
                Element::V(vec![
                    Element::A(Atom::Ope("begin")),
                    Element::V(vec![
                        Element::A(Atom::Ope("define")),
                        Element::A(Atom::Ident("abc".to_string())),
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
