use crate::token::*;
use std::iter::Peekable;

// 字句解析エラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    IllegalToken(char),
}

/// 字句解析
pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let mut input = input.chars().peekable();
    let mut tokens = vec![];
    while let Some(&c) = input.peek() {
        if c.is_digit(10) {
            let token = lex_int(&mut input);
            tokens.push(token);
        } else if c.is_ascii_whitespace() {
            // 空白は飛ばす
            input.next().unwrap();
        } else if c.is_alphabetic() {
            let token = lex_alpha(&mut input);
            tokens.push(token);
        } else {
            match c {
                '(' | ')' | '*' | '/' | '<' | '>' => {
                    tokens.push(Token::from_char(c).unwrap());
                    input.next().unwrap();
                }
                '+' | '-' => {
                    // 記号を刈り取る
                    input.next().unwrap();
                    // 次が数字のトークンであれば数として刈り取る
                    match input.peek() {
                        Some(next) => {
                            if next.is_digit(10) {
                                match lex_int(&mut input) {
                                    Token::INT(i) => {
                                        if c == '-' {
                                            tokens.push(Token::INT(-i))
                                        } else {
                                            tokens.push(Token::INT(i))
                                        }
                                    }
                                    _ => unreachable!(),
                                };
                            } else {
                                // "+@#.."
                                // 次の文字は刈り取らず，記号だけを取り込む
                                tokens.push(Token::from_char(c).unwrap());
                            }
                        }
                        // "+"
                        // 次の文字は刈り取らず，記号だけを取り込む
                        None => tokens.push(Token::from_char(c).unwrap()),
                    }
                }
                e => match Token::from_char(e) {
                    Ok(t) => tokens.push(t),
                    Err(_) => return Err(LexError::IllegalToken(e)),
                },
            }
        }
    }
    Ok(tokens)
}

fn lex_int<Tokens>(input: &mut Peekable<Tokens>) -> Token
where
    Tokens: Iterator<Item = char>,
{
    let mut buf = String::new();

    while let Some(&c) = input.peek() {
        if c.is_digit(10) {
            buf.push(input.next().unwrap());
        } else {
            break;
        }
    }
    let i = buf.parse::<i64>().unwrap();
    Token::INT(i)
}

fn lex_alpha<Tokens>(input: &mut Peekable<Tokens>) -> Token
where
    Tokens: Iterator<Item = char>,
{
    let mut s = String::new();

    while let Some(&c) = input.peek() {
        if c.is_alphabetic() {
            s.push(c);
            input.next().unwrap();
        } else {
            break;
        }
    }
    match s.as_str() {
        "begin" => Token::BEGIN,
        _ => Token::VAR(s),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::Token::*;

    #[test]
    fn test_lex() {
        let tests = vec![
            // (+ 1 (+ 2 3))
            (
                "(+ 1 (+ 2 3)) ",
                vec![
                    LPAREN,
                    PLUS,
                    INT(1),
                    LPAREN,
                    PLUS,
                    INT(2),
                    INT(3),
                    RPAREN,
                    RPAREN,
                ],
            ),
            ("() ", vec![LPAREN, RPAREN]),
            (
                "(begin abc) ",
                vec![LPAREN, BEGIN, VAR("abc".to_string()), RPAREN],
            ),
        ];
        for (input, expected) in tests.into_iter() {
            assert_eq!(expected, lex(input).unwrap());
        }
    }
    #[test]
    fn test_lex_ng() {
        let tests = vec![(" @ 1", LexError::IllegalToken('@'))];
        for (input, expected) in tests.into_iter() {
            match lex(input) {
                Ok(_) => panic!("expected err. but ok."),
                Err(e) => assert_eq!(expected, e),
            }
        }
    }
}
