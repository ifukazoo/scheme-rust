use crate::token::Token;
use std::fmt;
use std::iter::Peekable;

/// プログラムの要素
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Element {
    // ()なし
    Bare(Atom),
    // ()あり
    Paren(Vec<Element>),
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
impl fmt::Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Bare(a) => match a {
                Atom::App(app) => write!(f, "{}", app),
                Atom::Num(n) => write!(f, "{}", n),
                Atom::Bool(b) => write!(f, "{}", b),
                Atom::Ident(i) => write!(f, "{}", i),
            },
            Self::Paren(elements) => {
                let mut s = String::from("(");
                let mut sep = String::from("");
                for element in elements {
                    s.push_str(&format!("{}{}", sep, element));
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
pub fn parse_program(tokens: Vec<Token>) -> Result<Element, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    if tokens.len() == 0 {
        Err(ParseError::EmptyProgram)
    } else if tokens.len() == 1 {
        let a = parse_atom(&mut tokens)?;
        Ok(Element::Bare(a))
    } else {
        let elements = parse_array(&mut tokens)?;
        if tokens.peek().is_none() {
            Ok(Element::Paren(elements))
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
fn parse_array<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Vec<Element>, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    // `(`の刈り取り
    tokens.next().unwrap();

    let mut elements = vec![];
    while let Some(e) = tokens.peek() {
        match e {
            Token::RPAREN => break,
            Token::LPAREN => {
                let inner_elements = parse_array(tokens)?;
                elements.push(Element::Paren(inner_elements));
            }
            _ => {
                let atom = parse_atom(tokens)?;
                elements.push(Element::Bare(atom));
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
        Ok(elements)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer;

    #[test]
    fn test_parser() {
        let tests = vec![
            ("1", Element::Bare(Atom::Num(1))),
            ("()", Element::Paren(vec![])),
            (
                "(+ 1 2 )",
                Element::Paren(vec![
                    Element::Bare(Atom::App("+")),
                    Element::Bare(Atom::Num(1)),
                    Element::Bare(Atom::Num(2)),
                ]),
            ),
            (
                "(+ 1 (+ 2 3 ))",
                Element::Paren(vec![
                    Element::Bare(Atom::App("+")),
                    Element::Bare(Atom::Num(1)),
                    Element::Paren(vec![
                        Element::Bare(Atom::App("+")),
                        Element::Bare(Atom::Num(2)),
                        Element::Bare(Atom::Num(3)),
                    ]),
                ]),
            ),
            ("+", Element::Bare(Atom::App("+"))),
            (
                "(begin (+ 2 1) (+ 2 3))",
                Element::Paren(vec![
                    Element::Bare(Atom::App("begin")),
                    Element::Paren(vec![
                        Element::Bare(Atom::App("+")),
                        Element::Bare(Atom::Num(2)),
                        Element::Bare(Atom::Num(1)),
                    ]),
                    Element::Paren(vec![
                        Element::Bare(Atom::App("+")),
                        Element::Bare(Atom::Num(2)),
                        Element::Bare(Atom::Num(3)),
                    ]),
                ]),
            ),
            (
                r#"
                (begin
                    (define abc)
                )"#,
                Element::Paren(vec![
                    Element::Bare(Atom::App("begin")),
                    Element::Paren(vec![
                        Element::Bare(Atom::App("define")),
                        Element::Bare(Atom::Ident("abc".to_string())),
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
    fn test_element_disp() {
        let tests = vec![
            (Element::Bare(Atom::App("a")), "a"),
            (Element::Bare(Atom::Bool(true)), "true"),
            (
                Element::Paren(vec![
                    Element::Bare(Atom::App("list")),
                    Element::Bare(Atom::App("a")),
                    Element::Bare(Atom::Bool(true)),
                ]),
                "(list a true)",
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(expected, format!("{}", input))
        }
    }
}
