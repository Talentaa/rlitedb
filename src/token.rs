#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Numberic(f64),
    StringLiteral(String),

    Plus,  // +
    Minus, // -
    Mul,   // *
    Div,   // /

    NE, // != or <>
    LE, // <=
    GE, // >=
    LT, // <
    GT, // >
    EQ, // == or =

    Select, // SELECT
    Create, // CREATE
    Drop,   // DROP
    Insert, // INSERT
    Update, // UPDATE
    Table,  // TABLE
    Into,   // INTO
    From,   // FROM
    Values, // VALUES
    Where,  // WHERE
    And,    // AND
    Or,     // OR

    LParen,    // (
    RParen,    // )
    LBracket,  // [
    RBracket,  // ]
    LBrace,    // {
    RBrace,    // }
    Comma,     // ,
    SemiColon, // ;
}
