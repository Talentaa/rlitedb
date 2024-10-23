use crate::token::Token;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, tag_no_case, take},
    character::complete::{alpha1, alphanumeric1},
    combinator::{map, opt, recognize},
    multi::{many0_count, many1_count},
    sequence::{pair, tuple},
    IResult, Parser,
};

macro_rules! define_token {
    ($name:ident, $tag:expr, $token:expr) => {
        fn $name(input: &str) -> IResult<&str, Token> {
            map(tag($tag), |_| $token).parse(input)
        }
    };
}

macro_rules! define_token_no_case {
    ($name:ident, $tag:expr, $token:expr) => {
        #[allow(non_snake_case)]
        fn $name(input: &str) -> IResult<&str, Token> {
            map(tag_no_case($tag), |_| $token).parse(input)
        }
    };
}

// Punctuation mark
define_token!(double_quote, "\"", Token::DoubleQuote);
define_token!(percent, "%", Token::Percent);
define_token!(ampersand, "&", Token::Ampersand);
define_token!(quote, "'", Token::Quote);
define_token!(left_paren, "(", Token::LeftParen);
define_token!(right_paren, ")", Token::RightParen);
define_token!(asterisk, "*", Token::Asterisk);
define_token!(plus_sign, "+", Token::PlusSign);
define_token!(comma, ",", Token::Comma);
define_token!(minus_sign, "-", Token::MinusSign);
define_token!(period, ".", Token::Period);
define_token!(solidus, "/", Token::Solidus);
define_token!(colon, ":", Token::Colon);
define_token!(semicolon, ";", Token::Semicolon);
define_token!(less_than_operator, "<", Token::LessThanOperator);
define_token!(equals_operator, "=", Token::EqualsOperator);
define_token!(greater_than_operator, ">", Token::GreaterThanOperator);
define_token!(question_mark, "?", Token::QuestionMark);
define_token!(left_bracket, "[", Token::LeftBracket);
define_token!(left_bracket_trigraph, "??(", Token::LeftBracketTrigraph);
define_token!(right_bracket, "]", Token::RightBracket);
define_token!(right_bracket_trigraph, "??)", Token::RightBracketTrigraph);
define_token!(circumflex, "^", Token::Circumflex);
define_token!(underscore, "_", Token::Underscore);
define_token!(vertical_bar, "|", Token::VerticalBar);
define_token!(left_brace, "{", Token::LeftBrace);
define_token!(right_brace, "}", Token::RightBrace);
define_token!(not_equals_operator, "<>", Token::NotEqualsOperator);
define_token!(
    greater_than_or_equals_operator,
    ">=",
    Token::GreaterThanOrEqualsOperator
);
define_token!(
    less_than_or_equals_operator,
    ">=",
    Token::LessThanOrEqualsOperator
);
define_token!(concatenation_operator, "||", Token::ConcatenationOperator);
define_token!(right_arrow, "->", Token::RightArrow);
define_token!(double_colon, "::", Token::DoubleColon);
define_token!(double_period, "..", Token::DoublePeriod);

// Keywords
define_token_no_case!(ADD, "ADD", Token::Add);
define_token_no_case!(ALL, "ALL", Token::All);
define_token_no_case!(ALLOCATE, "ALLOCATE", Token::Allocate);
define_token_no_case!(ALTER, "ALTER", Token::Alter);
define_token_no_case!(AND, "AND", Token::And);
define_token_no_case!(ANY, "ANY", Token::Any);
define_token_no_case!(ARE, "ARE", Token::Are);
define_token_no_case!(ARRAY, "ARRAY", Token::Array);
define_token_no_case!(AS, "AS", Token::As);
define_token_no_case!(ASENSITIVE, "ASENSITIVE", Token::Asensitive);
define_token_no_case!(ASYMMETRIC, "ASYMMETRIC", Token::Asymmetric);
define_token_no_case!(AT, "AT", Token::At);
define_token_no_case!(ATOMIC, "ATOMIC", Token::Atomic);
define_token_no_case!(AUTHORIZATION, "AUTHORIZATION", Token::Authorization);
define_token_no_case!(BEGIN, "BEGIN", Token::Begin);
define_token_no_case!(BETWEEN, "BETWEEN", Token::Between);
define_token_no_case!(BIGINT, "BIGINT", Token::Bigint);
define_token_no_case!(BINARY, "BINARY", Token::Binary);
define_token_no_case!(BLOB, "BLOB", Token::Blob);
define_token_no_case!(BOOLEAN, "BOOLEAN", Token::Boolean);
define_token_no_case!(BOTH, "BOTH", Token::Both);
define_token_no_case!(BY, "BY", Token::By);
define_token_no_case!(CALL, "CALL", Token::Call);
define_token_no_case!(CALLED, "CALLED", Token::Called);
define_token_no_case!(CASCADED, "CASCADED", Token::Cascaded);
define_token_no_case!(CASE, "CASE", Token::Case);
define_token_no_case!(CAST, "CAST", Token::Cast);
define_token_no_case!(CHAR, "CHAR", Token::Char);
define_token_no_case!(CHARACTER, "CHARACTER", Token::Character);
define_token_no_case!(CHECK, "CHECK", Token::Check);
define_token_no_case!(CLOB, "CLOB", Token::Clob);
define_token_no_case!(CLOSE, "CLOSE", Token::Close);
define_token_no_case!(COLLATE, "COLLATE", Token::Collate);
define_token_no_case!(COLUMN, "COLUMN", Token::Column);
define_token_no_case!(COMMIT, "COMMIT", Token::Commit);
define_token_no_case!(CONNECT, "CONNECT", Token::Connect);
define_token_no_case!(CONSTRAINT, "CONSTRAINT", Token::Constraint);
define_token_no_case!(CONTINUE, "CONTINUE", Token::Continue);
define_token_no_case!(CORRESPONDING, "CORRESPONDING", Token::Corresponding);
define_token_no_case!(CREATE, "CREATE", Token::Create);
define_token_no_case!(CROSS, "CROSS", Token::Cross);
define_token_no_case!(CUBE, "CUBE", Token::Cube);
define_token_no_case!(CURRENT, "CURRENT", Token::Current);
define_token_no_case!(CURRENT_DATE, "CURRENT_DATE", Token::CurrentDate);
define_token_no_case!(
    CURRENT_DEFAULT_TRANSFORM_GROUP,
    "CURRENT_DEFAULT_TRANSFORM_GROUP",
    Token::CurrentDefaultTransformGroup
);
define_token_no_case!(CURRENT_PATH, "CURRENT_PATH", Token::CurrentPath);
define_token_no_case!(CURRENT_ROLE, "CURRENT_ROLE", Token::CurrentRole);
define_token_no_case!(CURRENT_TIME, "CURRENT_TIME", Token::CurrentTime);
define_token_no_case!(
    CURRENT_TIMESTAMP,
    "CURRENT_TIMESTAMP",
    Token::CurrentTimestamp
);
define_token_no_case!(
    CURRENT_TRANSFORM_GROUP_FOR_TYPE,
    "CURRENT_TRANSFORM_GROUP_FOR_TYPE",
    Token::CurrentTransformGroupForType
);
define_token_no_case!(CURRENT_USER, "CURRENT_USER", Token::CurrentUser);
define_token_no_case!(CURSOR, "CURSOR", Token::Cursor);
define_token_no_case!(CYCLE, "CYCLE", Token::Cycle);
define_token_no_case!(DATE, "DATE", Token::Date);
define_token_no_case!(DAY, "DAY", Token::Day);
define_token_no_case!(DEALLOCATE, "DEALLOCATE", Token::Deallocate);
define_token_no_case!(DEC, "DEC", Token::Dec);
define_token_no_case!(DECIMAL, "DECIMAL", Token::Decimal);
define_token_no_case!(DECLARE, "DECLARE", Token::Declare);
define_token_no_case!(DEFAULT, "DEFAULT", Token::Default);
define_token_no_case!(DELETE, "DELETE", Token::Delete);
define_token_no_case!(DEREF, "DEREF", Token::Deref);
define_token_no_case!(DESCRIBE, "DESCRIBE", Token::Describe);
define_token_no_case!(DETERMINISTIC, "DETERMINISTIC", Token::Deterministic);
define_token_no_case!(DISCONNECT, "DISCONNECT", Token::Disconnect);
define_token_no_case!(DISTINCT, "DISTINCT", Token::Distinct);
define_token_no_case!(DOUBLE, "DOUBLE", Token::Double);
define_token_no_case!(DROP, "DROP", Token::Drop);
define_token_no_case!(DYNAMIC, "DYNAMIC", Token::Dynamic);
define_token_no_case!(EACH, "EACH", Token::Each);
define_token_no_case!(ELEMENT, "ELEMENT", Token::Element);
define_token_no_case!(ELSE, "ELSE", Token::Else);
define_token_no_case!(END, "END", Token::End);
define_token_no_case!(END_EXEC, "END-EXEC", Token::EndExec);
define_token_no_case!(ESCAPE, "ESCAPE", Token::Escape);
define_token_no_case!(EXCEPT, "EXCEPT", Token::Except);
define_token_no_case!(EXEC, "EXEC", Token::Exec);
define_token_no_case!(EXECUTE, "EXECUTE", Token::Execute);
define_token_no_case!(EXISTS, "EXISTS", Token::Exists);
define_token_no_case!(EXTERNAL, "EXTERNAL", Token::External);
define_token_no_case!(FALSE, "FALSE", Token::False);
define_token_no_case!(FETCH, "FETCH", Token::Fetch);
define_token_no_case!(FILTER, "FILTER", Token::Filter);
define_token_no_case!(FLOAT, "FLOAT", Token::Float);
define_token_no_case!(FOR, "FOR", Token::For);
define_token_no_case!(FOREIGN, "FOREIGN", Token::Foreign);
define_token_no_case!(FREE, "FREE", Token::Free);
define_token_no_case!(FROM, "FROM", Token::From);
define_token_no_case!(FULL, "FULL", Token::Full);
define_token_no_case!(FUNCTION, "FUNCTION", Token::Function);
define_token_no_case!(GET, "GET", Token::Get);
define_token_no_case!(GLOBAL, "GLOBAL", Token::Global);
define_token_no_case!(GRANT, "GRANT", Token::Grant);
define_token_no_case!(GROUP, "GROUP", Token::Group);
define_token_no_case!(GROUPING, "GROUPING", Token::Grouping);
define_token_no_case!(HAVING, "HAVING", Token::Having);
define_token_no_case!(HOLD, "HOLD", Token::Hold);
define_token_no_case!(HOUR, "HOUR", Token::Hour);
define_token_no_case!(IDENTITY, "IDENTITY", Token::Identity);
define_token_no_case!(IMMEDIATE, "IMMEDIATE", Token::Immediate);
define_token_no_case!(IN, "IN", Token::In);
define_token_no_case!(INDICATOR, "INDICATOR", Token::Indicator);
define_token_no_case!(INNER, "INNER", Token::Inner);
define_token_no_case!(INOUT, "INOUT", Token::Inout);
define_token_no_case!(INPUT, "INPUT", Token::Input);
define_token_no_case!(INSENSITIVE, "INSENSITIVE", Token::Insensitive);
define_token_no_case!(INSERT, "INSERT", Token::Insert);
define_token_no_case!(INT, "INT", Token::Int);
define_token_no_case!(INTEGER, "INTEGER", Token::Integer);
define_token_no_case!(INTERSECT, "INTERSECT", Token::Intersect);
define_token_no_case!(INTERVAL, "INTERVAL", Token::Interval);
define_token_no_case!(INTO, "INTO", Token::Into);
define_token_no_case!(IS, "IS", Token::Is);
define_token_no_case!(ISOLATION, "ISOLATION", Token::Isolation);
define_token_no_case!(JOIN, "JOIN", Token::Join);
define_token_no_case!(LANGUAGE, "LANGUAGE", Token::Language);
define_token_no_case!(LARGE, "LARGE", Token::Large);
define_token_no_case!(LATERAL, "LATERAL", Token::Lateral);
define_token_no_case!(LEADING, "LEADING", Token::Leading);
define_token_no_case!(LEFT, "LEFT", Token::Left);
define_token_no_case!(LIKE, "LIKE", Token::Like);
define_token_no_case!(LOCAL, "LOCAL", Token::Local);
define_token_no_case!(LOCALTIME, "LOCALTIME", Token::Localtime);
define_token_no_case!(LOCALTIMESTAMP, "LOCALTIMESTAMP", Token::Localtimestamp);
define_token_no_case!(MATCH, "MATCH", Token::Match);
define_token_no_case!(MEMBER, "MEMBER", Token::Member);
define_token_no_case!(MERGE, "MERGE", Token::Merge);
define_token_no_case!(METHOD, "METHOD", Token::Method);
define_token_no_case!(MINUTE, "MINUTE", Token::Minute);
define_token_no_case!(MODIFIES, "MODIFIES", Token::Modifies);
define_token_no_case!(MODULE, "MODULE", Token::Module);
define_token_no_case!(MONTH, "MONTH", Token::Month);
define_token_no_case!(MULTISET, "MULTISET", Token::Multiset);
define_token_no_case!(NATIONAL, "NATIONAL", Token::National);
define_token_no_case!(NATURAL, "NATURAL", Token::Natural);
define_token_no_case!(NCHAR, "NCHAR", Token::Nchar);
define_token_no_case!(NCLOB, "NCLOB", Token::Nclob);
define_token_no_case!(NEW, "NEW", Token::New);
define_token_no_case!(NO, "NO", Token::No);
define_token_no_case!(NONE, "NONE", Token::None);
define_token_no_case!(NOT, "NOT", Token::Not);
define_token_no_case!(NULL, "NULL", Token::Null);
define_token_no_case!(NUMERIC, "NUMERIC", Token::Numeric);
define_token_no_case!(OF, "OF", Token::Of);
define_token_no_case!(OLD, "OLD", Token::Old);
define_token_no_case!(ON, "ON", Token::On);
define_token_no_case!(ONLY, "ONLY", Token::Only);
define_token_no_case!(OPEN, "OPEN", Token::Open);
define_token_no_case!(OR, "OR", Token::Or);
define_token_no_case!(ORDER, "ORDER", Token::Order);
define_token_no_case!(OUT, "OUT", Token::Out);
define_token_no_case!(OUTER, "OUTER", Token::Outer);
define_token_no_case!(OUTPUT, "OUTPUT", Token::Output);
define_token_no_case!(OVER, "OVER", Token::Over);
define_token_no_case!(OVERLAPS, "OVERLAPS", Token::Overlaps);
define_token_no_case!(PARAMETER, "PARAMETER", Token::Parameter);
define_token_no_case!(PARTITION, "PARTITION", Token::Partition);
define_token_no_case!(PRECISION, "PRECISION", Token::Precision);
define_token_no_case!(PREPARE, "PREPARE", Token::Prepare);
define_token_no_case!(PRIMARY, "PRIMARY", Token::Primary);
define_token_no_case!(PROCEDURE, "PROCEDURE", Token::Procedure);
define_token_no_case!(RANGE, "RANGE", Token::Range);
define_token_no_case!(READS, "READS", Token::Reads);
define_token_no_case!(REAL, "REAL", Token::Real);
define_token_no_case!(RECURSIVE, "RECURSIVE", Token::Recursive);
define_token_no_case!(REF, "REF", Token::Ref);
define_token_no_case!(REFERENCES, "REFERENCES", Token::References);
define_token_no_case!(REFERENCING, "REFERENCING", Token::Referencing);
define_token_no_case!(REGR_AVGX, "REGR_AVGX", Token::RegrAvgx);
define_token_no_case!(REGR_AVGY, "REGR_AVGY", Token::RegrAvgy);
define_token_no_case!(REGR_COUNT, "REGR_COUNT", Token::RegrCount);
define_token_no_case!(REGR_INTERCEPT, "REGR_INTERCEPT", Token::RegrIntercept);
define_token_no_case!(REGR_R2, "REGR_R2", Token::RegrR2);
define_token_no_case!(REGR_SLOPE, "REGR_SLOPE", Token::RegrSlope);
define_token_no_case!(REGR_SXX, "REGR_SXX", Token::RegrSxx);
define_token_no_case!(REGR_SXY, "REGR_SXY", Token::RegrSxy);
define_token_no_case!(REGR_SYY, "REGR_SYY", Token::RegrSyy);
define_token_no_case!(RELEASE, "RELEASE", Token::Release);
define_token_no_case!(RESULT, "RESULT", Token::Result);
define_token_no_case!(RETURN, "RETURN", Token::Return);
define_token_no_case!(RETURNS, "RETURNS", Token::Returns);
define_token_no_case!(REVOKE, "REVOKE", Token::Revoke);
define_token_no_case!(RIGHT, "RIGHT", Token::Right);
define_token_no_case!(ROLLBACK, "ROLLBACK", Token::Rollback);
define_token_no_case!(ROLLUP, "ROLLUP", Token::Rollup);
define_token_no_case!(ROW, "ROW", Token::Row);
define_token_no_case!(ROWS, "ROWS", Token::Rows);
define_token_no_case!(SAVEPOINT, "SAVEPOINT", Token::Savepoint);
define_token_no_case!(SCROLL, "SCROLL", Token::Scroll);
define_token_no_case!(SEARCH, "SEARCH", Token::Search);
define_token_no_case!(SECOND, "SECOND", Token::Second);
define_token_no_case!(SELECT, "SELECT", Token::Select);
define_token_no_case!(SENSITIVE, "SENSITIVE", Token::Sensitive);
define_token_no_case!(SESSION_USER, "SESSION_USER", Token::SessionUser);
define_token_no_case!(SET, "SET", Token::Set);
define_token_no_case!(SIMILAR, "SIMILAR", Token::Similar);
define_token_no_case!(SMALLINT, "SMALLINT", Token::Smallint);
define_token_no_case!(SOME, "SOME", Token::Some);
define_token_no_case!(SPECIFIC, "SPECIFIC", Token::Specific);
define_token_no_case!(SPECIFICTYPE, "SPECIFICTYPE", Token::Specifictype);
define_token_no_case!(SQL, "SQL", Token::Sql);
define_token_no_case!(SQLEXCEPTION, "SQLEXCEPTION", Token::Sqlexception);
define_token_no_case!(SQLSTATE, "SQLSTATE", Token::Sqlstate);
define_token_no_case!(SQLWARNING, "SQLWARNING", Token::Sqlwarning);
define_token_no_case!(START, "START", Token::Start);
define_token_no_case!(STATIC, "STATIC", Token::Static);
define_token_no_case!(SUBMULTISET, "SUBMULTISET", Token::Submultiset);
define_token_no_case!(SYMMETRIC, "SYMMETRIC", Token::Symmetric);
define_token_no_case!(SYSTEM, "SYSTEM", Token::System);
define_token_no_case!(SYSTEM_USER, "SYSTEM_USER", Token::SystemUser);
define_token_no_case!(TABLE, "TABLE", Token::Table);
define_token_no_case!(THEN, "THEN", Token::Then);
define_token_no_case!(TIME, "TIME", Token::Time);
define_token_no_case!(TIMESTAMP, "TIMESTAMP", Token::Timestamp);
define_token_no_case!(TIMEZONE_HOUR, "TIMEZONE_HOUR", Token::TimezoneHour);
define_token_no_case!(TIMEZONE_MINUTE, "TIMEZONE_MINUTE", Token::TimezoneMinute);
define_token_no_case!(TO, "TO", Token::To);
define_token_no_case!(TRAILING, "TRAILING", Token::Trailing);
define_token_no_case!(TRANSLATION, "TRANSLATION", Token::Translation);
define_token_no_case!(TREAT, "TREAT", Token::Treat);
define_token_no_case!(TRIGGER, "TRIGGER", Token::Trigger);
define_token_no_case!(TRUE, "TRUE", Token::True);
define_token_no_case!(UESCAPE, "UESCAPE", Token::Uescape);
define_token_no_case!(UNION, "UNION", Token::Union);
define_token_no_case!(UNIQUE, "UNIQUE", Token::Unique);
define_token_no_case!(UNKNOWN, "UNKNOWN", Token::Unknown);
define_token_no_case!(UNNEST, "UNNEST", Token::Unnest);
define_token_no_case!(UPDATE, "UPDATE", Token::Update);
define_token_no_case!(UPPER, "UPPER", Token::Upper);
define_token_no_case!(USER, "USER", Token::User);
define_token_no_case!(USING, "USING", Token::Using);
define_token_no_case!(VALUE, "VALUE", Token::Value);
define_token_no_case!(VALUES, "VALUES", Token::Values);
define_token_no_case!(VAR_POP, "VAR_POP", Token::VarPop);
define_token_no_case!(VAR_SAMP, "VAR_SAMP", Token::VarSamp);
define_token_no_case!(VARCHAR, "VARCHAR", Token::Varchar);
define_token_no_case!(VARYING, "VARYING", Token::Varying);
define_token_no_case!(WHEN, "WHEN", Token::When);
define_token_no_case!(WHENEVER, "WHENEVER", Token::Whenever);
define_token_no_case!(WHERE, "WHERE", Token::Where);
define_token_no_case!(WIDTH_BUCKET, "WIDTH_BUCKET", Token::WidthBucket);
define_token_no_case!(WINDOW, "WINDOW", Token::Window);
define_token_no_case!(WITH, "WITH", Token::With);
define_token_no_case!(WITHIN, "WITHIN", Token::Within);
define_token_no_case!(WITHOUT, "WITHOUT", Token::Without);
define_token_no_case!(YEAR, "YEAR", Token::Year);

fn identifier(input: &str) -> IResult<&str, Token> {
    map(actual_identifier, |ident: &str| {
        Token::Identifier(ident.to_string())
    })
    .parse(input)
}

fn regular_identifier(input: &str) -> IResult<&str, &str> {
    identifier_body.parse(input)
}

fn identifier_body(input: &str) -> IResult<&str, &str> {
    recognize(pair(identifier_start, opt(identifier_part))).parse(input)
}

fn identifier_start(input: &str) -> IResult<&str, &str> {
    alt((alpha1, recognize(underscore))).parse(input)
}

fn identifier_part(input: &str) -> IResult<&str, &str> {
    alt((identifier_start, identifier_extend)).parse(input)
}

fn identifier_extend(input: &str) -> IResult<&str, &str> {
    recognize(many0_count(alt((alphanumeric1, recognize(underscore))))).parse(input)
}

// TODO
fn actual_identifier(input: &str) -> IResult<&str, &str> {
    alt((regular_identifier, delimited_identifier)).parse(input)
}

fn delimited_identifier(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        double_quote,
        delimited_identifier_body,
        double_quote,
    )))
    .parse(input)
}

fn delimited_identifier_body(input: &str) -> IResult<&str, &str> {
    recognize(many1_count(delimited_identifier_part)).parse(input)
}

fn delimited_identifier_part(input: &str) -> IResult<&str, &str> {
    alt((nondoublequote_character, doublequote_symbol)).parse(input)
}

fn nondoublequote_character(input: &str) -> IResult<&str, &str> {
    is_not("\"").and_then(take(1usize)).parse(input)
}

fn doublequote_symbol(input: &str) -> IResult<&str, &str> {
    recognize(pair(double_quote, double_quote)).parse(input)
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::identifier;

    #[test]
    fn test_identifier_1() {
        let input = "\"\"\"\"";
        let (_, res) = identifier(input).unwrap();
        let expected = Token::Identifier("\"\"\"\"".to_string());
        assert_eq!(res, expected);
    }

    #[test]
    fn test_identifier_2() {
        let input = "\"\"\"\"\"\"";
        let (_, res) = identifier(input).unwrap();
        let expected = Token::Identifier("\"\"\"\"\"\"".to_string());
        assert_eq!(res, expected);
    }

    #[test]
    fn test_identifier_3() {
        let input = "\"identifier\"";
        let (_, res) = identifier(input).unwrap();
        let expected = Token::Identifier("\"identifier\"".to_string());
        assert_eq!(res, expected);
    }

    #[test]
    fn test_identifier_4() {
        let input = "\"\"\"identifier\"\"\"";
        let (_, res) = identifier(input).unwrap();
        let expected = Token::Identifier("\"\"\"identifier\"\"\"".to_string());
        assert_eq!(res, expected);
    }
    #[test]
    fn test_identifier_5() {
        let input = "\"identifier1\"\"identifier2\"";
        let (_, res) = identifier(input).unwrap();
        let expected = Token::Identifier("\"identifier1\"\"identifier2\"".to_string());
        assert_eq!(res, expected);
    }
}
