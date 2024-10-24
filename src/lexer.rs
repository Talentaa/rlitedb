use crate::token::Token;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, tag_no_case, take},
    character::complete::{alpha1, alphanumeric1, digit1, multispace1, newline, space1},
    combinator::{map, opt, recognize},
    multi::{many0_count, many1_count},
    sequence::{pair, tuple},
    IResult, Parser,
};

macro_rules! define_parse {
    ($name:ident, $tag:expr) => {
        #[allow(dead_code)]
        fn $name(input: &str) -> IResult<&str, &str> {
            tag($tag).parse(input)
        }
    };
}

macro_rules! define_token_no_case {
    ($name:ident, $tag:expr, $token:expr) => {
        #[allow(non_snake_case, dead_code)]
        fn $name(input: &str) -> IResult<&str, Token> {
            map(tag_no_case($tag), |_| $token).parse(input)
        }
    };
}

// Punctuation mark
define_parse!(double_quote, "\"");
define_parse!(percent, "%");
define_parse!(ampersand, "&");
define_parse!(quote, "'");
define_parse!(left_paren, "(");
define_parse!(right_paren, ")");
define_parse!(asterisk, "*");
define_parse!(plus_sign, "+");
define_parse!(comma, ",");
define_parse!(minus_sign, "-");
define_parse!(period, ".");
define_parse!(solidus, "/");
define_parse!(colon, ":");
define_parse!(semicolon, ";");
define_parse!(less_than_operator, "<");
define_parse!(equals_operator, "=");
define_parse!(greater_than_operator, ">");
define_parse!(question_mark, "?");
define_parse!(left_bracket, "[");
define_parse!(left_bracket_trigraph, "??(");
define_parse!(right_bracket, "]");
define_parse!(right_bracket_trigraph, "??)");
define_parse!(circumflex, "^");
define_parse!(underscore, "_");
define_parse!(vertical_bar, "|");
define_parse!(left_brace, "{");
define_parse!(right_brace, "}");
define_parse!(not_equals_operator, "<>");
define_parse!(greater_than_or_equals_operator, ">=");
define_parse!(less_than_or_equals_operator, "<=");
define_parse!(concatenation_operator, "||");
define_parse!(right_arrow, "->");
define_parse!(double_colon, "::");
define_parse!(double_period, "..");

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

// Token
pub fn token(input: &str) -> IResult<&str, Token> {
    // nondelimiter_token | delimiter_token
    alt((nondelimiter_token, delimiter_token)).parse(input)
}

// TODO
fn nondelimiter_token(input: &str) -> IResult<&str, Token> {
    // regular_identifier
    // | key_word
    // | unsigned_numeric_literal
    // | national_character_string_literal
    // | bit_string_literal
    // | hex_string_literal
    // | large_object_length_token
    // | multiplier
    alt((
        key_word,
        map(multiplier, |multi: &str| match multi {
            "K" => Token::LargeObjectLength("1K".to_string()),
            "M" => Token::LargeObjectLength("1M".to_string()),
            "G" => Token::LargeObjectLength("1G".to_string()),
            _ => unreachable!(),
        }),
        map(large_object_length_token, |lg: &str| {
            Token::LargeObjectLength(lg.to_string())
        }),
        map(unsigned_numeric_literal, |liter: &str| {
            Token::Literal(liter.to_string())
        }),
        map(regular_identifier, |ident: &str| {
            Token::Identifier(ident.to_string())
        }),
    ))
    .parse(input)
}

fn regular_identifier(input: &str) -> IResult<&str, &str> {
    // identifier_body
    identifier_body.parse(input)
}

fn identifier_body(input: &str) -> IResult<&str, &str> {
    // identifier_start identifier_part?
    recognize(pair(identifier_start, opt(identifier_part))).parse(input)
}

fn identifier_part(input: &str) -> IResult<&str, &str> {
    // identifier_start | identifier_extend
    alt((identifier_start, identifier_extend)).parse(input)
}

fn identifier_start(input: &str) -> IResult<&str, &str> {
    alt((alpha1, recognize(underscore))).parse(input)
}

fn identifier_extend(input: &str) -> IResult<&str, &str> {
    recognize(many0_count(alt((alphanumeric1, recognize(underscore))))).parse(input)
}

fn large_object_length_token(input: &str) -> IResult<&str, &str> {
    // digit ... multiplier
    recognize(pair(digit1, multiplier)).parse(input)
}

fn multiplier(input: &str) -> IResult<&str, &str> {
    // 'K' | 'M' | 'G'
    alt((tag("K"), tag("M"), tag("G"))).parse(input)
}

fn delimited_identifier(input: &str) -> IResult<&str, &str> {
    // double_quote delimited_identifier_body double_quote
    recognize(tuple((
        double_quote,
        delimited_identifier_body,
        double_quote,
    )))
    .parse(input)
}

fn delimited_identifier_body(input: &str) -> IResult<&str, &str> {
    // delimited_identifier_part ...
    recognize(many1_count(delimited_identifier_part)).parse(input)
}

fn delimited_identifier_part(input: &str) -> IResult<&str, &str> {
    // nondoublequote_character | doublequote_symbol
    alt((nondoublequote_character, doublequote_symbol)).parse(input)
}

// Literal Token
fn literal(input: &str) -> IResult<&str, &str> {
    // signed_numeric_literal | general_literal
    alt((signed_numeric_literal, general_literal)).parse(input)
}

// TODO
fn general_literal(input: &str) -> IResult<&str, &str> {
    // character_string_literal
    // | national_character_string_literal
    // | Unicode_character_string_literal
    // | binary_string_literal
    // | datetime_literal
    // | interval_literal
    // | boolean_literal
    character_string_literal.parse(input)
}

// TODO
fn character_string_literal(input: &str) -> IResult<&str, &str> {
    // ( introducer character_set_specification )?
    // quote ( character_representation )? quote
    // ( separator quote ( character_representation )? quote )*

    recognize(tuple((
        tuple((quote, opt(character_representation), quote)),
        many0_count(tuple((
            separator,
            quote,
            opt(character_representation),
            quote,
        ))),
    )))
    .parse(input)
}

fn introducer(input: &str) -> IResult<&str, &str> {
    // underscore
    recognize(underscore).parse(input)
}

fn character_representation(input: &str) -> IResult<&str, &str> {
    // nonquote_character | quote_symbol
    alt((nonquote_character, quote_symbol)).parse(input)
}

fn nonquote_character(input: &str) -> IResult<&str, &str> {
    is_not("'").and_then(take(1usize)).parse(input)
}

fn quote_symbol(input: &str) -> IResult<&str, &str> {
    // quote quote
    recognize(pair(quote, quote)).parse(input)
}

fn signed_numeric_literal(input: &str) -> IResult<&str, &str> {
    // sign? unsigned_numeric_literal
    recognize(pair(opt(sign), unsigned_numeric_literal)).parse(input)
}

fn unsigned_numeric_literal(input: &str) -> IResult<&str, &str> {
    // exact_numeric_literal | approximate_numeric_literal
    alt((exact_numeric_literal, approximate_numeric_literal)).parse(input)
}

fn exact_numeric_literal(input: &str) -> IResult<&str, &str> {
    alt((
        // unsigned_integer ( period unsigned_integer? )?
        recognize(pair(
            unsigned_integer,
            opt(pair(period, opt(unsigned_integer))),
        )),
        // period unsigned_integer
        recognize(pair(period, unsigned_integer)),
    ))
    .parse(input)
}

fn sign(input: &str) -> IResult<&str, &str> {
    // plus_sign | minus_sign
    recognize(alt((plus_sign, minus_sign))).parse(input)
}

fn approximate_numeric_literal(input: &str) -> IResult<&str, &str> {
    // mantissa E exponent
    recognize(tuple((mantissa, tag("E"), exponent))).parse(input)
}

fn mantissa(input: &str) -> IResult<&str, &str> {
    // exact_numeric_literal
    exact_numeric_literal.parse(input)
}

fn exponent(input: &str) -> IResult<&str, &str> {
    // signed_integer
    signed_integer.parse(input)
}

fn signed_integer(input: &str) -> IResult<&str, &str> {
    // sign? unsigned_integer
    recognize(pair(opt(sign), unsigned_integer)).parse(input)
}

fn unsigned_integer(input: &str) -> IResult<&str, &str> {
    // digit ...
    digit1.parse(input)
}

// Identifier Token
fn identifier(input: &str) -> IResult<&str, Token> {
    map(actual_identifier, |ident: &str| {
        Token::Identifier(ident.to_string())
    })
    .parse(input)
}

fn actual_identifier(input: &str) -> IResult<&str, &str> {
    alt((regular_identifier, delimited_identifier)).parse(input)
}

fn nondoublequote_character(input: &str) -> IResult<&str, &str> {
    is_not("\"").and_then(take(1usize)).parse(input)
}

fn doublequote_symbol(input: &str) -> IResult<&str, &str> {
    recognize(pair(double_quote, double_quote)).parse(input)
}

// TODO
fn delimiter_token(input: &str) -> IResult<&str, Token> {
    // character_string_literal
    // | date_string
    // | time_string
    // | timestamp_string
    // | interval_string
    // | delimited_identifier
    // | Unicode_delimited_identifier
    // | SQL_special_character
    // | not_equals_operator
    // | greater_than_or_equals_operator
    // | less_than_or_equals_operator
    // | concatenation_operator
    // | right_arrow
    // | left_bracket_trigraph
    // | right_bracket_trigraph
    // | double_colon
    // | double_period

    alt((
        map(character_string_literal, |v: &str| {
            Token::Literal(v.to_string())
        }),
        map(delimited_identifier, |v: &str| {
            Token::Identifier(v.to_string())
        }),
        map(not_equals_operator, |_| Token::NotEqualsOperator),
        map(greater_than_or_equals_operator, |_| {
            Token::GreaterThanOrEqualsOperator
        }),
        map(less_than_or_equals_operator, |_| {
            Token::LessThanOrEqualsOperator
        }),
        map(concatenation_operator, |_| Token::ConcatenationOperator),
        map(right_arrow, |_| Token::RightArrow),
        map(left_bracket_trigraph, |_| Token::LeftBracketTrigraph),
        map(right_bracket_trigraph, |_| Token::RightBracketTrigraph),
        map(double_colon, |_| Token::DoubleColon),
        map(double_period, |_| Token::DoublePeriod),
    ))
    .parse(input)
}

fn separator(input: &str) -> IResult<&str, &str> {
    // ( comment | white_space )*
    recognize(many0_count(alt((comment, multispace1)))).parse(input)
}

fn comment(input: &str) -> IResult<&str, &str> {
    // simple_comment | bracketed_comment
    alt((simple_comment, bracketed_comment)).parse(input)
}

fn simple_comment(input: &str) -> IResult<&str, &str> {
    // simple_comment_introducer comment_character* newline
    recognize(tuple((
        simple_comment_introducer,
        many0_count(comment_character),
        newline,
    )))
    .parse(input)
}

fn simple_comment_introducer(input: &str) -> IResult<&str, &str> {
    // minus_sign minus_sign  minus_sign*
    recognize(tuple((minus_sign, minus_sign, many0_count(minus_sign)))).parse(input)
}

fn bracketed_comment(input: &str) -> IResult<&str, &str> {
    // bracketed_comment_introducer bracketed_comment_contents bracketed_comment_terminator
    recognize(tuple((
        bracketed_comment_introducer,
        bracketed_comment_contents,
        bracketed_comment_terminator,
    )))
    .parse(input)
}

fn bracketed_comment_introducer(input: &str) -> IResult<&str, &str> {
    // solidus asterisk
    recognize(pair(solidus, asterisk)).parse(input)
}

fn bracketed_comment_terminator(input: &str) -> IResult<&str, &str> {
    // asterisk solidus
    recognize(pair(asterisk, solidus)).parse(input)
}

fn bracketed_comment_contents(input: &str) -> IResult<&str, &str> {
    // ( comment_character | separator )*
    recognize(many0_count(alt((comment_character, separator)))).parse(input)
}

fn comment_character(input: &str) -> IResult<&str, &str> {
    // ( comment_character | separator )*
    alt((nonquote_character, quote)).parse(input)
}

// TODO
fn key_word(input: &str) -> IResult<&str, Token> {
    // reserved_word | non_reserved_word
    reserved_word.parse(input)
}

// TODO
fn reserved_word(input: &str) -> IResult<&str, Token> {
    // so many key word
    alt((ADD, ALL)).parse(input)
}

#[cfg(test)]
mod tests {
    use crate::{lexer::token, token::Token};

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

    #[test]
    fn test_token_not_equals_operator() {
        let input = "<>";
        let (_, res) = token(input).unwrap();
        assert_eq!(Token::NotEqualsOperator, res);
    }

    #[test]
    fn test_token_greater_than_or_equals_operator() {
        let input = ">=";
        let (_, res) = token(input).unwrap();
        assert_eq!(Token::GreaterThanOrEqualsOperator, res);
    }

    #[test]
    fn test_token_less_than_or_equals_operator() {
        let input = "<=";
        let (_, res) = token(input).unwrap();
        assert_eq!(Token::LessThanOrEqualsOperator, res);
    }

    #[test]
    fn test_token_concatenation_operator() {
        let input = "||";
        let (_, res) = token(input).unwrap();
        assert_eq!(Token::ConcatenationOperator, res);
    }

    #[test]
    fn test_token_right_arrow() {
        let input = "->";
        let (_, res) = token(input).unwrap();
        assert_eq!(Token::RightArrow, res);
    }

    #[test]
    fn test_token_left_bracket_trigraph() {
        let input = "??(";
        let (_, res) = token(input).unwrap();
        assert_eq!(Token::LeftBracketTrigraph, res);
    }

    #[test]
    fn test_token_right_bracket_trigraph() {
        let input = "??)";
        let (_, res) = token(input).unwrap();
        assert_eq!(Token::RightBracketTrigraph, res);
    }

    #[test]
    fn test_token_double_colon() {
        let input = "::";
        let (_, res) = token(input).unwrap();
        assert_eq!(Token::DoubleColon, res);
    }

    #[test]
    fn test_token_double_period() {
        let input = "..";
        let (_, res) = token(input).unwrap();
        assert_eq!(Token::DoublePeriod, res);
    }
}
