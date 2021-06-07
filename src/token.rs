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
    /// 変数
    VAR(String),
    // STRING 'string'
}

impl Token {
    pub fn from_char(c: char) -> Result<Self, ()> {
        match c {
            ')' => Ok(Self::RPAREN),
            '(' => Ok(Self::LPAREN),
            '*' => Ok(Self::ASTER),
            '+' => Ok(Self::PLUS),
            '-' => Ok(Self::MINUS),
            '/' => Ok(Self::SLASH),
            '>' => Ok(Self::GT),
            '<' => Ok(Self::LT),
            _ => Err(()),
        }
    }
}
