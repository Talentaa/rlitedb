use crate::token::Token;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{alpha1, digit1, multispace0},
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

define_token!(plus_operator, "+", Token::Plus);
define_token!(minus_operator, "-", Token::Minus);
define_token!(mul_operator, "*", Token::Mul);
define_token!(div_operator, "/", Token::Div);
define_token!(lt_operator, "<", Token::LT);
define_token!(gt_operator, ">", Token::GT);
define_token!(le_operator, "<=", Token::LE);
define_token!(ge_operator, ">=", Token::GE);
define_token_alt!(ne_operator, ("!=", "<>"), Token::NE);
define_token_alt!(eq_operator, ("==", "="), Token::EQ);

fn operator(input: &str) -> IResult<&str, Token> {
    alt((
        plus_operator,
        minus_operator,
        mul_operator,
        div_operator,
        lt_operator,
        gt_operator,
        le_operator,
        ge_operator,
        ne_operator,
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
define_token_no_case!(and_keyword, "and", Token::AND);
define_token_no_case!(or_keyword, "or", Token::OR);

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

fn integer(input: &str) -> IResult<&str, Token> {
    map_res(digit1, |s: &str| {
        s.parse::<i64>().map(|i| Token::Integer(i))
    })
    .parse(input)
}

fn token(input: &str) -> IResult<&str, Token> {
    let floatpoint = map(double, |f: f64| Token::FloatPoint(f));

    alt((operator, punctuation, keyword, ident, floatpoint, integer)).parse(input)
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
            Token::Integer(10),
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
            Token::Integer(10),
            Token::AND,
            Token::Ident("weight".to_string()),
            Token::LT,
            Token::Integer(60),
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
            Token::Integer(10),
            Token::OR,
            Token::Ident("weight".to_string()),
            Token::LT,
            Token::Integer(60),
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
            Token::FloatPoint(1.0),
            Token::SemiColon,
        ];

        assert_eq!(res, expected);
    }
}
