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
    // STRING 'string'
}

impl Token {
    pub fn new(c: char) -> Result<Self, ()> {
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
