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
        } else if c.is_alphabetic() || c == '#' {
            // #t と #f を取り込む
            let token = lex_string(&mut input);
            tokens.push(token);
        } else {
            match c {
                '(' | ')' | '*' | '/' | '<' | '>' => {
                    tokens.push(Token::from_char(c));
                    input.next().unwrap();
                }
                '+' | '-' => {
                    let token = lex_sign_head(&mut input);
                    tokens.push(token);
                }
                e => return Err(LexError::IllegalToken(e)),
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

fn lex_string<Tokens>(input: &mut Peekable<Tokens>) -> Token
where
    Tokens: Iterator<Item = char>,
{
    // 先頭文字を刈り取り
    let c = input.next().unwrap();
    let mut s = String::from(c);

    while let Some(&c) = input.peek() {
        // set!, eq? などがあるのでこの段階では一緒に取り込んでいる．
        if c.is_alphanumeric() || c == '!' || c == '?' || c == '*' {
            s.push(c);
            input.next().unwrap();
        } else {
            break;
        }
    }
    match s.as_str() {
        "begin" => Token::BEGIN,
        "define" => Token::DEFINE,
        "set!" => Token::SET,
        "cons" => Token::CONS,
        "car" => Token::CAR,
        "cdr" => Token::CDR,
        "list" => Token::LIST,
        "#t" => Token::TRUE,
        "#f" => Token::FALSE,
        "equal?" => Token::EQUAL,
        "not" => Token::NOT,
        "zero?" => Token::ISZERO,
        "if" => Token::IF,
        "cond" => Token::COND,
        "else" => Token::ELSE,
        "let" => Token::LET,
        "let*" => Token::LETA,
        "letrec" => Token::LETREC,
        "lambda" => Token::LAMBDA,
        _ => Token::VAR(s),
    }
}
// +,-記号が先頭にあるトークンの解析
fn lex_sign_head<Tokens>(input: &mut Peekable<Tokens>) -> Token
where
    Tokens: Iterator<Item = char>,
{
    // 正負の記号を刈り取る
    let sign = input.next().unwrap();

    // -1, +1 など次が数字のトークンであれば数として刈り取る
    match input.peek() {
        Some(next) => {
            if next.is_digit(10) {
                if let Token::INT(i) = lex_int(input) {
                    if sign == '-' {
                        Token::INT(-i)
                    } else {
                        Token::INT(i)
                    }
                } else {
                    // 必ず数字にマッチするのでここにはこない
                    unreachable!()
                }
            } else {
                // +,- 記号のトークンだけを返す
                Token::from_char(sign)
            }
        }
        // +,- 記号のトークン
        None => Token::from_char(sign),
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
            ("+1", vec![INT(1)]),
            ("-1", vec![INT(-1)]),
            ("+0", vec![INT(0)]),
            ("-0", vec![INT(0)]),
            ("-", vec![MINUS]),
            ("-c", vec![MINUS, VAR("c".to_string())]),
            ("() ", vec![LPAREN, RPAREN]),
            (
                "(begin abc) ",
                vec![LPAREN, BEGIN, VAR("abc".to_string()), RPAREN],
            ),
            (
                "(define abc) ",
                vec![LPAREN, DEFINE, VAR("abc".to_string()), RPAREN],
            ),
            (
                "(set! abc 1) ",
                vec![LPAREN, SET, VAR("abc".to_string()), INT(1), RPAREN],
            ),
            (
                "(get! abc 1) ",
                vec![
                    LPAREN,
                    VAR("get!".to_string()),
                    VAR("abc".to_string()),
                    INT(1),
                    RPAREN,
                ],
            ),
            ("(cons 1 2) ", vec![LPAREN, CONS, INT(1), INT(2), RPAREN]),
            ("#t", vec![TRUE]),
            (" #t ", vec![TRUE]),
            ("#f", vec![FALSE]),
            (
                "(lambda (a) a) ",
                vec![
                    LPAREN,
                    LAMBDA,
                    LPAREN,
                    VAR("a".to_string()),
                    RPAREN,
                    VAR("a".to_string()),
                    RPAREN,
                ],
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
