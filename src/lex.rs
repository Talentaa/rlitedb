use crate::token::Token;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{alpha1, char, digit1, multispace0},
    combinator::{map, map_res},
    multi::many0,
    number::complete::double,
    sequence::delimited,
    IResult, Parser,
};

macro_rules! define_token {
    ($name:ident, $tag:expr, $token:expr) => {
        fn $name(input: &str) -> IResult<&str, Token> {
            map(tag($tag), |_| $token).parse(input)
        }
    };
}

macro_rules! define_token_alt {
  ($name:ident, ($($tag:expr),+), $token:expr) => {
      fn $name(input: &str) -> IResult<&str, Token> {
          map(alt(($(tag($tag)),+)), |_| $token).parse(input)
      }
  };
}

macro_rules! define_token_no_case {
    ($name:ident, $tag:expr, $token:expr) => {
        fn $name(input: &str) -> IResult<&str, Token> {
            map(tag_no_case($tag), |_| $token).parse(input)
        }
    };
}

define_token!(mul_operator, "*", Token::Mul);
define_token!(div_operator, "/", Token::Div);
define_token!(plus_operator, "+", Token::Plus);
define_token!(minus_operator, "-", Token::Minus);
define_token_alt!(ne_operator, ("!=", "<>"), Token::NE);
define_token!(le_operator, "<=", Token::LE);
define_token!(ge_operator, ">=", Token::GE);
define_token!(lt_operator, "<", Token::LT);
define_token!(gt_operator, ">", Token::GT);
define_token_alt!(eq_operator, ("==", "="), Token::EQ);

fn operator(input: &str) -> IResult<&str, Token> {
    alt((
        mul_operator,
        div_operator,
        plus_operator,
        minus_operator,
        ne_operator,
        le_operator,
        ge_operator,
        lt_operator,
        gt_operator,
        eq_operator,
    ))
    .parse(input)
}

define_token!(lparen_punctuation, "(", Token::LParen);
define_token!(rparen_punctuation, ")", Token::RParen);
define_token!(lbracket_punctuation, "[", Token::LBracket);
define_token!(rbracket_punctuation, "]", Token::RBracket);
define_token!(lbrace_punctuation, "{", Token::LBrace);
define_token!(rbrace_punctuation, "}", Token::RBrace);
define_token!(comma_punctuation, ",", Token::Comma);
define_token!(semi_colon_punctuation, ";", Token::SemiColon);

fn punctuation(input: &str) -> IResult<&str, Token> {
    alt((
        lparen_punctuation,
        rparen_punctuation,
        lbracket_punctuation,
        rbracket_punctuation,
        lbrace_punctuation,
        rbrace_punctuation,
        comma_punctuation,
        semi_colon_punctuation,
    ))
    .parse(input)
}

define_token_no_case!(select_keyword, "select", Token::Select);
define_token_no_case!(create_keyword, "create", Token::Create);
define_token_no_case!(drop_keyword, "drop", Token::Drop);
define_token_no_case!(insert_keyword, "insert", Token::Insert);
define_token_no_case!(update_keyword, "update", Token::Update);
define_token_no_case!(table_keyword, "table", Token::Table);
define_token_no_case!(into_keyword, "into", Token::Into);
define_token_no_case!(from_keyword, "from", Token::From);
define_token_no_case!(values_keyword, "values", Token::Values);
define_token_no_case!(where_keyword, "where", Token::Where);
define_token_no_case!(and_keyword, "and", Token::And);
define_token_no_case!(or_keyword, "or", Token::Or);

fn keyword(input: &str) -> IResult<&str, Token> {
    alt((
        select_keyword,
        create_keyword,
        drop_keyword,
        insert_keyword,
        update_keyword,
        table_keyword,
        into_keyword,
        from_keyword,
        values_keyword,
        where_keyword,
        and_keyword,
        or_keyword,
    ))
    .parse(input)
}

fn ident(input: &str) -> IResult<&str, Token> {
    map(alpha1, |s: &str| Token::Ident(s.to_string())).parse(input)
}

fn numberic(input: &str) -> IResult<&str, Token> {
    map(double, |f: f64| Token::Numberic(f)).parse(input)
}

fn string_literal(input: &str) -> IResult<&str, Token> {
    map(
        alt((
            delimited(char('\''), alpha1, char('\'')),
            delimited(char('"'), alpha1, char('"')),
        )),
        |string: &str| Token::StringLiteral(string.to_owned()),
    )
    .parse(input)
}

fn token(input: &str) -> IResult<&str, Token> {
    alt((
        operator,
        punctuation,
        keyword,
        ident,
        numberic,
        string_literal,
    ))
    .parse(input)
}

fn tokens(input: &str) -> IResult<&str, Vec<Token>> {
    many0(delimited(multispace0, token, multispace0)).parse(input)
}

pub struct Lexer;

impl Lexer {
    pub fn get_tokens(input: &str) -> Vec<Token> {
        tokens(input).unwrap().1
    }
}

#[cfg(test)]
mod tests {
    use super::tokens;
    use crate::token::Token;

    #[test]
    fn test_token_1() {
        let input = "+-*/";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![Token::Plus, Token::Minus, Token::Mul, Token::Div];
        assert_eq!(res, expected);
    }

    #[test]
    fn test_token_2() {
        let input = "!= <> <= >= < > == =";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![
            Token::NE,
            Token::NE,
            Token::LE,
            Token::GE,
            Token::LT,
            Token::GT,
            Token::EQ,
            Token::EQ,
        ];
        assert_eq!(res, expected);
    }

    #[test]
    fn test_token_3() {
        let input = "SELECT CREATE DROP INSERT UPDATE TABLE INTO FROM VALUES WHERE AND OR";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![
            Token::Select,
            Token::Create,
            Token::Drop,
            Token::Insert,
            Token::Update,
            Token::Table,
            Token::Into,
            Token::From,
            Token::Values,
            Token::Where,
            Token::And,
            Token::Or,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_string_literal() {
        let input = "'abcd' \"efgh\"";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![
            Token::StringLiteral("abcd".to_string()),
            Token::StringLiteral("efgh".to_string()),
        ];
        assert_eq!(res, expected);
    }

    #[test]
    fn test_token_4() {
        let input = "()[]{},;";
        let (_, res) = tokens(input).unwrap();
        let expected: Vec<Token> = vec![
            Token::LParen,
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::SemiColon,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_select_1() {
        let input = "SELECT * FROM users;";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![
            Token::Select,
            Token::Mul,
            Token::From,
            Token::Ident("users".to_string()),
            Token::SemiColon,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_select_2() {
        let input = "SELECT id,name FROM users;";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![
            Token::Select,
            Token::Ident("id".to_string()),
            Token::Comma,
            Token::Ident("name".to_string()),
            Token::From,
            Token::Ident("users".to_string()),
            Token::SemiColon,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_select_3() {
        let input = "SELECT id,name FROM users where age > 10;";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![
            Token::Select,
            Token::Ident("id".to_string()),
            Token::Comma,
            Token::Ident("name".to_string()),
            Token::From,
            Token::Ident("users".to_string()),
            Token::Where,
            Token::Ident("age".to_string()),
            Token::GT,
            Token::Numberic(10f64),
            Token::SemiColon,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_select_4() {
        let input = "SELECT id, name FROM users where age > 10 AND weight < 60;";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![
            Token::Select,
            Token::Ident("id".to_string()),
            Token::Comma,
            Token::Ident("name".to_string()),
            Token::From,
            Token::Ident("users".to_string()),
            Token::Where,
            Token::Ident("age".to_string()),
            Token::GT,
            Token::Numberic(10f64),
            Token::And,
            Token::Ident("weight".to_string()),
            Token::LT,
            Token::Numberic(60f64),
            Token::SemiColon,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_select_5() {
        let input = "SELECT * FROM users where age > 10 OR weight < 60;";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![
            Token::Select,
            Token::Mul,
            Token::From,
            Token::Ident("users".to_string()),
            Token::Where,
            Token::Ident("age".to_string()),
            Token::GT,
            Token::Numberic(10f64),
            Token::Or,
            Token::Ident("weight".to_string()),
            Token::LT,
            Token::Numberic(60f64),
            Token::SemiColon,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_select_6() {
        let input = "SELECT * FROM users where rate < 1.0;";
        let (_, res) = tokens(input).unwrap();
        let expected = vec![
            Token::Select,
            Token::Mul,
            Token::From,
            Token::Ident("users".to_string()),
            Token::Where,
            Token::Ident("rate".to_string()),
            Token::LT,
            Token::Numberic(1.0),
            Token::SemiColon,
        ];

        assert_eq!(res, expected);
    }
}
