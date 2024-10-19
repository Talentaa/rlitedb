#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Numberic(f64),

    Plus,  // +
    Minus, // -
    Mul,   // *
    Div,   // /

    LT, // <
    GT, // >
    LE, // <=
    GE, // >=
    NE, // != or <>
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
    AND,    // AND
    OR,     // OR

    LParen,    // (
    RParen,    // )
    LBracket,  // [
    RBracket,  // ]
    LBrace,    // {
    RBrace,    // }
    Comma,     // ,
    SemiColon, // ;
}
