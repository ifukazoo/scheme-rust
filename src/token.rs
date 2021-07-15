/// Token
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LPAREN,
    RPAREN,
    INT(i64),
    PLUS,
    MINUS,
    ASTER,
    SLASH,
    GT,
    LT,
    BEGIN,
    DEFINE,
    SET,
    CONS,
    CAR,
    CDR,
    LIST,
    TRUE,
    FALSE,
    EQUAL,
    NOT,
    ISZERO,
    IF,
    COND,
    ELSE,
    LET,
    LETA,
    LETREC,
    LAMBDA,
    /// 変数
    VAR(String),
    // STRING 'string'
}

impl Token {
    pub fn from_char(c: char) -> Self {
        match c {
            ')' => Self::RPAREN,
            '(' => Self::LPAREN,
            '*' => Self::ASTER,
            '+' => Self::PLUS,
            '-' => Self::MINUS,
            '/' => Self::SLASH,
            '>' => Self::GT,
            '<' => Self::LT,
            // 呼び出し側でチェックは終わっている
            _ => unreachable!(),
        }
    }
}
