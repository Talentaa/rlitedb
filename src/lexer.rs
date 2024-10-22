use crate::token::Token;
use nom::{bytes::complete::tag, combinator::map, IResult, Parser};

macro_rules! define_token {
    ($name:ident, $tag:expr, $token:expr) => {
        #[allow(non_snake_case)]
        fn $name(input: &str) -> IResult<&str, Token> {
            map(tag($tag), |_| $token).parse(input)
        }
    };
}

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

define_token!(ADD, "ADD", Token::Add);
define_token!(ALL, "ALL", Token::All);
define_token!(ALLOCATE, "ALLOCATE", Token::Allocate);
define_token!(ALTER, "ALTER", Token::Alter);
define_token!(AND, "AND", Token::And);
define_token!(ANY, "ANY", Token::Any);
define_token!(ARE, "ARE", Token::Are);
define_token!(ARRAY, "ARRAY", Token::Array);
define_token!(AS, "AS", Token::As);
define_token!(ASENSITIVE, "ASENSITIVE", Token::Asensitive);
define_token!(ASYMMETRIC, "ASYMMETRIC", Token::Asymmetric);
define_token!(AT, "AT", Token::At);
define_token!(ATOMIC, "ATOMIC", Token::Atomic);
define_token!(AUTHORIZATION, "AUTHORIZATION", Token::Authorization);
define_token!(BEGIN, "BEGIN", Token::Begin);
define_token!(BETWEEN, "BETWEEN", Token::Between);
define_token!(BIGINT, "BIGINT", Token::Bigint);
define_token!(BINARY, "BINARY", Token::Binary);
define_token!(BLOB, "BLOB", Token::Blob);
define_token!(BOOLEAN, "BOOLEAN", Token::Boolean);
define_token!(BOTH, "BOTH", Token::Both);
define_token!(BY, "BY", Token::By);
define_token!(CALL, "CALL", Token::Call);
define_token!(CALLED, "CALLED", Token::Called);
define_token!(CASCADED, "CASCADED", Token::Cascaded);
define_token!(CASE, "CASE", Token::Case);
define_token!(CAST, "CAST", Token::Cast);
define_token!(CHAR, "CHAR", Token::Char);
define_token!(CHARACTER, "CHARACTER", Token::Character);
define_token!(CHECK, "CHECK", Token::Check);
define_token!(CLOB, "CLOB", Token::Clob);
define_token!(CLOSE, "CLOSE", Token::Close);
define_token!(COLLATE, "COLLATE", Token::Collate);
define_token!(COLUMN, "COLUMN", Token::Column);
define_token!(COMMIT, "COMMIT", Token::Commit);
define_token!(CONNECT, "CONNECT", Token::Connect);
define_token!(CONSTRAINT, "CONSTRAINT", Token::Constraint);
define_token!(CONTINUE, "CONTINUE", Token::Continue);
define_token!(CORRESPONDING, "CORRESPONDING", Token::Corresponding);
define_token!(CREATE, "CREATE", Token::Create);
define_token!(CROSS, "CROSS", Token::Cross);
define_token!(CUBE, "CUBE", Token::Cube);
define_token!(CURRENT, "CURRENT", Token::Current);
define_token!(CURRENT_DATE, "CURRENT_DATE", Token::CurrentDate);
define_token!(
    CURRENT_DEFAULT_TRANSFORM_GROUP,
    "CURRENT_DEFAULT_TRANSFORM_GROUP",
    Token::CurrentDefaultTransformGroup
);
define_token!(CURRENT_PATH, "CURRENT_PATH", Token::CurrentPath);
define_token!(CURRENT_ROLE, "CURRENT_ROLE", Token::CurrentRole);
define_token!(CURRENT_TIME, "CURRENT_TIME", Token::CurrentTime);
define_token!(
    CURRENT_TIMESTAMP,
    "CURRENT_TIMESTAMP",
    Token::CurrentTimestamp
);
define_token!(
    CURRENT_TRANSFORM_GROUP_FOR_TYPE,
    "CURRENT_TRANSFORM_GROUP_FOR_TYPE",
    Token::CurrentTransformGroupForType
);
define_token!(CURRENT_USER, "CURRENT_USER", Token::CurrentUser);
define_token!(CURSOR, "CURSOR", Token::Cursor);
define_token!(CYCLE, "CYCLE", Token::Cycle);
define_token!(DATE, "DATE", Token::Date);
define_token!(DAY, "DAY", Token::Day);
define_token!(DEALLOCATE, "DEALLOCATE", Token::Deallocate);
define_token!(DEC, "DEC", Token::Dec);
define_token!(DECIMAL, "DECIMAL", Token::Decimal);
define_token!(DECLARE, "DECLARE", Token::Declare);
define_token!(DEFAULT, "DEFAULT", Token::Default);
define_token!(DELETE, "DELETE", Token::Delete);
define_token!(DEREF, "DEREF", Token::Deref);
define_token!(DESCRIBE, "DESCRIBE", Token::Describe);
define_token!(DETERMINISTIC, "DETERMINISTIC", Token::Deterministic);
define_token!(DISCONNECT, "DISCONNECT", Token::Disconnect);
define_token!(DISTINCT, "DISTINCT", Token::Distinct);
define_token!(DOUBLE, "DOUBLE", Token::Double);
define_token!(DROP, "DROP", Token::Drop);
define_token!(DYNAMIC, "DYNAMIC", Token::Dynamic);
define_token!(EACH, "EACH", Token::Each);
define_token!(ELEMENT, "ELEMENT", Token::Element);
define_token!(ELSE, "ELSE", Token::Else);
define_token!(END, "END", Token::End);
define_token!(END_EXEC, "END-EXEC", Token::EndExec);
define_token!(ESCAPE, "ESCAPE", Token::Escape);
define_token!(EXCEPT, "EXCEPT", Token::Except);
define_token!(EXEC, "EXEC", Token::Exec);
define_token!(EXECUTE, "EXECUTE", Token::Execute);
define_token!(EXISTS, "EXISTS", Token::Exists);
define_token!(EXTERNAL, "EXTERNAL", Token::External);
define_token!(FALSE, "FALSE", Token::False);
define_token!(FETCH, "FETCH", Token::Fetch);
define_token!(FILTER, "FILTER", Token::Filter);
define_token!(FLOAT, "FLOAT", Token::Float);
define_token!(FOR, "FOR", Token::For);
define_token!(FOREIGN, "FOREIGN", Token::Foreign);
define_token!(FREE, "FREE", Token::Free);
define_token!(FROM, "FROM", Token::From);
define_token!(FULL, "FULL", Token::Full);
define_token!(FUNCTION, "FUNCTION", Token::Function);
define_token!(GET, "GET", Token::Get);
define_token!(GLOBAL, "GLOBAL", Token::Global);
define_token!(GRANT, "GRANT", Token::Grant);
define_token!(GROUP, "GROUP", Token::Group);
define_token!(GROUPING, "GROUPING", Token::Grouping);
define_token!(HAVING, "HAVING", Token::Having);
define_token!(HOLD, "HOLD", Token::Hold);
define_token!(HOUR, "HOUR", Token::Hour);
define_token!(IDENTITY, "IDENTITY", Token::Identity);
define_token!(IMMEDIATE, "IMMEDIATE", Token::Immediate);
define_token!(IN, "IN", Token::In);
define_token!(INDICATOR, "INDICATOR", Token::Indicator);
define_token!(INNER, "INNER", Token::Inner);
define_token!(INOUT, "INOUT", Token::Inout);
define_token!(INPUT, "INPUT", Token::Input);
define_token!(INSENSITIVE, "INSENSITIVE", Token::Insensitive);
define_token!(INSERT, "INSERT", Token::Insert);
define_token!(INT, "INT", Token::Int);
define_token!(INTEGER, "INTEGER", Token::Integer);
define_token!(INTERSECT, "INTERSECT", Token::Intersect);
define_token!(INTERVAL, "INTERVAL", Token::Interval);
define_token!(INTO, "INTO", Token::Into);
define_token!(IS, "IS", Token::Is);
define_token!(ISOLATION, "ISOLATION", Token::Isolation);
define_token!(JOIN, "JOIN", Token::Join);
define_token!(LANGUAGE, "LANGUAGE", Token::Language);
define_token!(LARGE, "LARGE", Token::Large);
define_token!(LATERAL, "LATERAL", Token::Lateral);
define_token!(LEADING, "LEADING", Token::Leading);
define_token!(LEFT, "LEFT", Token::Left);
define_token!(LIKE, "LIKE", Token::Like);
define_token!(LOCAL, "LOCAL", Token::Local);
define_token!(LOCALTIME, "LOCALTIME", Token::Localtime);
define_token!(LOCALTIMESTAMP, "LOCALTIMESTAMP", Token::Localtimestamp);
define_token!(MATCH, "MATCH", Token::Match);
define_token!(MEMBER, "MEMBER", Token::Member);
define_token!(MERGE, "MERGE", Token::Merge);
define_token!(METHOD, "METHOD", Token::Method);
define_token!(MINUTE, "MINUTE", Token::Minute);
define_token!(MODIFIES, "MODIFIES", Token::Modifies);
define_token!(MODULE, "MODULE", Token::Module);
define_token!(MONTH, "MONTH", Token::Month);
define_token!(MULTISET, "MULTISET", Token::Multiset);
define_token!(NATIONAL, "NATIONAL", Token::National);
define_token!(NATURAL, "NATURAL", Token::Natural);
define_token!(NCHAR, "NCHAR", Token::Nchar);
define_token!(NCLOB, "NCLOB", Token::Nclob);
define_token!(NEW, "NEW", Token::New);
define_token!(NO, "NO", Token::No);
define_token!(NONE, "NONE", Token::None);
define_token!(NOT, "NOT", Token::Not);
define_token!(NULL, "NULL", Token::Null);
define_token!(NUMERIC, "NUMERIC", Token::Numeric);
define_token!(OF, "OF", Token::Of);
define_token!(OLD, "OLD", Token::Old);
define_token!(ON, "ON", Token::On);
define_token!(ONLY, "ONLY", Token::Only);
define_token!(OPEN, "OPEN", Token::Open);
define_token!(OR, "OR", Token::Or);
define_token!(ORDER, "ORDER", Token::Order);
define_token!(OUT, "OUT", Token::Out);
define_token!(OUTER, "OUTER", Token::Outer);
define_token!(OUTPUT, "OUTPUT", Token::Output);
define_token!(OVER, "OVER", Token::Over);
define_token!(OVERLAPS, "OVERLAPS", Token::Overlaps);
define_token!(PARAMETER, "PARAMETER", Token::Parameter);
define_token!(PARTITION, "PARTITION", Token::Partition);
define_token!(PRECISION, "PRECISION", Token::Precision);
define_token!(PREPARE, "PREPARE", Token::Prepare);
define_token!(PRIMARY, "PRIMARY", Token::Primary);
define_token!(PROCEDURE, "PROCEDURE", Token::Procedure);
define_token!(RANGE, "RANGE", Token::Range);
define_token!(READS, "READS", Token::Reads);
define_token!(REAL, "REAL", Token::Real);
define_token!(RECURSIVE, "RECURSIVE", Token::Recursive);
define_token!(REF, "REF", Token::Ref);
define_token!(REFERENCES, "REFERENCES", Token::References);
define_token!(REFERENCING, "REFERENCING", Token::Referencing);
define_token!(REGR_AVGX, "REGR_AVGX", Token::RegrAvgx);
define_token!(REGR_AVGY, "REGR_AVGY", Token::RegrAvgy);
define_token!(REGR_COUNT, "REGR_COUNT", Token::RegrCount);
define_token!(REGR_INTERCEPT, "REGR_INTERCEPT", Token::RegrIntercept);
define_token!(REGR_R2, "REGR_R2", Token::RegrR2);
define_token!(REGR_SLOPE, "REGR_SLOPE", Token::RegrSlope);
define_token!(REGR_SXX, "REGR_SXX", Token::RegrSxx);
define_token!(REGR_SXY, "REGR_SXY", Token::RegrSxy);
define_token!(REGR_SYY, "REGR_SYY", Token::RegrSyy);
define_token!(RELEASE, "RELEASE", Token::Release);
define_token!(RESULT, "RESULT", Token::Result);
define_token!(RETURN, "RETURN", Token::Return);
define_token!(RETURNS, "RETURNS", Token::Returns);
define_token!(REVOKE, "REVOKE", Token::Revoke);
define_token!(RIGHT, "RIGHT", Token::Right);
define_token!(ROLLBACK, "ROLLBACK", Token::Rollback);
define_token!(ROLLUP, "ROLLUP", Token::Rollup);
define_token!(ROW, "ROW", Token::Row);
define_token!(ROWS, "ROWS", Token::Rows);
define_token!(SAVEPOINT, "SAVEPOINT", Token::Savepoint);
define_token!(SCROLL, "SCROLL", Token::Scroll);
define_token!(SEARCH, "SEARCH", Token::Search);
define_token!(SECOND, "SECOND", Token::Second);
define_token!(SELECT, "SELECT", Token::Select);
define_token!(SENSITIVE, "SENSITIVE", Token::Sensitive);
define_token!(SESSION_USER, "SESSION_USER", Token::SessionUser);
define_token!(SET, "SET", Token::Set);
define_token!(SIMILAR, "SIMILAR", Token::Similar);
define_token!(SMALLINT, "SMALLINT", Token::Smallint);
define_token!(SOME, "SOME", Token::Some);
define_token!(SPECIFIC, "SPECIFIC", Token::Specific);
define_token!(SPECIFICTYPE, "SPECIFICTYPE", Token::Specifictype);
define_token!(SQL, "SQL", Token::Sql);
define_token!(SQLEXCEPTION, "SQLEXCEPTION", Token::Sqlexception);
define_token!(SQLSTATE, "SQLSTATE", Token::Sqlstate);
define_token!(SQLWARNING, "SQLWARNING", Token::Sqlwarning);
define_token!(START, "START", Token::Start);
define_token!(STATIC, "STATIC", Token::Static);
define_token!(SUBMULTISET, "SUBMULTISET", Token::Submultiset);
define_token!(SYMMETRIC, "SYMMETRIC", Token::Symmetric);
define_token!(SYSTEM, "SYSTEM", Token::System);
define_token!(SYSTEM_USER, "SYSTEM_USER", Token::SystemUser);
define_token!(TABLE, "TABLE", Token::Table);
define_token!(THEN, "THEN", Token::Then);
define_token!(TIME, "TIME", Token::Time);
define_token!(TIMESTAMP, "TIMESTAMP", Token::Timestamp);
define_token!(TIMEZONE_HOUR, "TIMEZONE_HOUR", Token::TimezoneHour);
define_token!(TIMEZONE_MINUTE, "TIMEZONE_MINUTE", Token::TimezoneMinute);
define_token!(TO, "TO", Token::To);
define_token!(TRAILING, "TRAILING", Token::Trailing);
define_token!(TRANSLATION, "TRANSLATION", Token::Translation);
define_token!(TREAT, "TREAT", Token::Treat);
define_token!(TRIGGER, "TRIGGER", Token::Trigger);
define_token!(TRUE, "TRUE", Token::True);
define_token!(UESCAPE, "UESCAPE", Token::Uescape);
define_token!(UNION, "UNION", Token::Union);
define_token!(UNIQUE, "UNIQUE", Token::Unique);
define_token!(UNKNOWN, "UNKNOWN", Token::Unknown);
define_token!(UNNEST, "UNNEST", Token::Unnest);
define_token!(UPDATE, "UPDATE", Token::Update);
define_token!(UPPER, "UPPER", Token::Upper);
define_token!(USER, "USER", Token::User);
define_token!(USING, "USING", Token::Using);
define_token!(VALUE, "VALUE", Token::Value);
define_token!(VALUES, "VALUES", Token::Values);
define_token!(VAR_POP, "VAR_POP", Token::VarPop);
define_token!(VAR_SAMP, "VAR_SAMP", Token::VarSamp);
define_token!(VARCHAR, "VARCHAR", Token::Varchar);
define_token!(VARYING, "VARYING", Token::Varying);
define_token!(WHEN, "WHEN", Token::When);
define_token!(WHENEVER, "WHENEVER", Token::Whenever);
define_token!(WHERE, "WHERE", Token::Where);
define_token!(WIDTH_BUCKET, "WIDTH_BUCKET", Token::WidthBucket);
define_token!(WINDOW, "WINDOW", Token::Window);
define_token!(WITH, "WITH", Token::With);
define_token!(WITHIN, "WITHIN", Token::Within);
define_token!(WITHOUT, "WITHOUT", Token::Without);
define_token!(YEAR, "YEAR", Token::Year);
