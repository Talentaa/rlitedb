use std::{
    iter::Enumerate,
    ops::{Range, RangeFrom, RangeFull, RangeTo},
};

use nom::{InputIter, InputLength, InputTake, Needed, Slice};

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),           // Identifier
    Space,                        // here shoule be empty
    DoubleQuote,                  // "
    Percent,                      // %
    Ampersand,                    // &
    Quote,                        // '
    LeftParen,                    // (
    RightParen,                   // )
    Asterisk,                     // *
    PlusSign,                     // +
    Comma,                        // ,
    MinusSign,                    // -
    Period,                       // .
    Solidus,                      // /
    Colon,                        // :
    Semicolon,                    // ;
    LessThanOperator,             // <
    EqualsOperator,               // =
    GreaterThanOperator,          // >
    QuestionMark,                 // ?
    LeftBracket,                  // [
    LeftBracketTrigraph,          // ??(
    RightBracket,                 // ]
    RightBracketTrigraph,         // ??)
    Circumflex,                   // ^
    Underscore,                   // _
    VerticalBar,                  // |
    LeftBrace,                    // {
    RightBrace,                   // }
    NotEqualsOperator,            // <>
    GreaterThanOrEqualsOperator,  // >=
    LessThanOrEqualsOperator,     // <=
    ConcatenationOperator,        // ||
    RightArrow,                   // ->
    DoubleColon,                  // ::
    DoublePeriod,                 // ..
    Add,                          // ADD
    All,                          // ALL
    Allocate,                     // ALLOCATE
    Alter,                        // ALTER
    And,                          // AND
    Any,                          // ANY
    Are,                          // ARE
    Array,                        // ARRAY
    As,                           // AS
    Asensitive,                   // ASENSITIVE
    Asymmetric,                   // ASYMMETRIC
    At,                           // AT
    Atomic,                       // ATOMIC
    Authorization,                // AUTHORIZATION
    Begin,                        // BEGIN
    Between,                      // BETWEEN
    Bigint,                       // BIGINT
    Binary,                       // BINARY
    Blob,                         // BLOB
    Boolean,                      // BOOLEAN
    Both,                         // BOTH
    By,                           // BY
    Call,                         // CALL
    Called,                       // CALLED
    Cascaded,                     // CASCADED
    Case,                         // CASE
    Cast,                         // CAST
    Char,                         // CHAR
    Character,                    // CHARACTER
    Check,                        // CHECK
    Clob,                         // CLOB
    Close,                        // CLOSE
    Collate,                      // COLLATE
    Column,                       // COLUMN
    Commit,                       // COMMIT
    Connect,                      // CONNECT
    Constraint,                   // CONSTRAINT
    Continue,                     // CONTINUE
    Corresponding,                // CORRESPONDING
    Create,                       // CREATE
    Cross,                        // CROSS
    Cube,                         // CUBE
    Current,                      // CURRENT
    CurrentDate,                  // CURRENT_DATE
    CurrentDefaultTransformGroup, // CURRENT_DEFAULT_TRANSFORM_GROUP
    CurrentPath,                  // CURRENT_PATH
    CurrentRole,                  // CURRENT_ROLE
    CurrentTime,                  // CURRENT_TIME
    CurrentTimestamp,             // CURRENT_TIMESTAMP
    CurrentTransformGroupForType, // CURRENT_TRANSFORM_GROUP_FOR_TYPE
    CurrentUser,                  // CURRENT_USER
    Cursor,                       // CURSOR
    Cycle,                        // CYCLE
    Date,                         // DATE
    Day,                          // DAY
    Deallocate,                   // DEALLOCATE
    Dec,                          // DEC
    Decimal,                      // DECIMAL
    Declare,                      // DECLARE
    Default,                      // DEFAULT
    Delete,                       // DELETE
    Deref,                        // DEREF
    Describe,                     // DESCRIBE
    Deterministic,                // DETERMINISTIC
    Disconnect,                   // DISCONNECT
    Distinct,                     // DISTINCT
    Double,                       // DOUBLE
    Drop,                         // DROP
    Dynamic,                      // DYNAMIC
    Each,                         // EACH
    Element,                      // ELEMENT
    Else,                         // ELSE
    End,                          // END
    EndExec,                      // END-EXEC
    Escape,                       // ESCAPE
    Except,                       // EXCEPT
    Exec,                         // EXEC
    Execute,                      // EXECUTE
    Exists,                       // EXISTS
    External,                     // EXTERNAL
    False,                        // FALSE
    Fetch,                        // FETCH
    Filter,                       // FILTER
    Float,                        // FLOAT
    For,                          // FOR
    Foreign,                      // FOREIGN
    Free,                         // FREE
    From,                         // FROM
    Full,                         // FULL
    Function,                     // FUNCTION
    Get,                          // GET
    Global,                       // GLOBAL
    Grant,                        // GRANT
    Group,                        // GROUP
    Grouping,                     // GROUPING
    Having,                       // HAVING
    Hold,                         // HOLD
    Hour,                         // HOUR
    Identity,                     // IDENTITY
    Immediate,                    // IMMEDIATE
    In,                           // IN
    Indicator,                    // INDICATOR
    Inner,                        // INNER
    Inout,                        // INOUT
    Input,                        // INPUT
    Insensitive,                  // INSENSITIVE
    Insert,                       // INSERT
    Int,                          // INT
    Integer,                      // INTEGER
    Intersect,                    // INTERSECT
    Interval,                     // INTERVAL
    Into,                         // INTO
    Is,                           // IS
    Isolation,                    // ISOLATION
    Join,                         // JOIN
    Language,                     // LANGUAGE
    Large,                        // LARGE
    Lateral,                      // LATERAL
    Leading,                      // LEADING
    Left,                         // LEFT
    Like,                         // LIKE
    Local,                        // LOCAL
    Localtime,                    // LOCALTIME
    Localtimestamp,               // LOCALTIMESTAMP
    Match,                        // MATCH
    Member,                       // MEMBER
    Merge,                        // MERGE
    Method,                       // METHOD
    Minute,                       // MINUTE
    Modifies,                     // MODIFIES
    Module,                       // MODULE
    Month,                        // MONTH
    Multiset,                     // MULTISET
    National,                     // NATIONAL
    Natural,                      // NATURAL
    Nchar,                        // NCHAR
    Nclob,                        // NCLOB
    New,                          // NEW
    No,                           // NO
    None,                         // NONE
    Not,                          // NOT
    Null,                         // NULL
    Numeric,                      // NUMERIC
    Of,                           // OF
    Old,                          // OLD
    On,                           // ON
    Only,                         // ONLY
    Open,                         // OPEN
    Or,                           // OR
    Order,                        // ORDER
    Out,                          // OUT
    Outer,                        // OUTER
    Output,                       // OUTPUT
    Over,                         // OVER
    Overlaps,                     // OVERLAPS
    Parameter,                    // PARAMETER
    Partition,                    // PARTITION
    Precision,                    // PRECISION
    Prepare,                      // PREPARE
    Primary,                      // PRIMARY
    Procedure,                    // PROCEDURE
    Range,                        // RANGE
    Reads,                        // READS
    Real,                         // REAL
    Recursive,                    // RECURSIVE
    Ref,                          // REF
    References,                   // REFERENCES
    Referencing,                  // REFERENCING
    RegrAvgx,                     // REGR_AVGX
    RegrAvgy,                     // REGR_AVGY
    RegrCount,                    // REGR_COUNT
    RegrIntercept,                // REGR_INTERCEPT
    RegrR2,                       // REGR_R2
    RegrSlope,                    // REGR_SLOPE
    RegrSxx,                      // REGR_SXX
    RegrSxy,                      // REGR_SXY
    RegrSyy,                      // REGR_SYY
    Release,                      // RELEASE
    Result,                       // RESULT
    Return,                       // RETURN
    Returns,                      // RETURNS
    Revoke,                       // REVOKE
    Right,                        // RIGHT
    Rollback,                     // ROLLBACK
    Rollup,                       // ROLLUP
    Row,                          // ROW
    Rows,                         // ROWS
    Savepoint,                    // SAVEPOINT
    Scroll,                       // SCROLL
    Search,                       // SEARCH
    Second,                       // SECOND
    Select,                       // SELECT
    Sensitive,                    // SENSITIVE
    SessionUser,                  // SESSION_USER
    Set,                          // SET
    Similar,                      // SIMILAR
    Smallint,                     // SMALLINT
    Some,                         // SOME
    Specific,                     // SPECIFIC
    Specifictype,                 // SPECIFICTYPE
    Sql,                          // SQL
    Sqlexception,                 // SQLEXCEPTION
    Sqlstate,                     // SQLSTATE
    Sqlwarning,                   // SQLWARNING
    Start,                        // START
    Static,                       // STATIC
    Submultiset,                  // SUBMULTISET
    Symmetric,                    // SYMMETRIC
    System,                       // SYSTEM
    SystemUser,                   // SYSTEM_USER
    Table,                        // TABLE
    Then,                         // THEN
    Time,                         // TIME
    Timestamp,                    // TIMESTAMP
    TimezoneHour,                 // TIMEZONE_HOUR
    TimezoneMinute,               // TIMEZONE_MINUTE
    To,                           // TO
    Trailing,                     // TRAILING
    Translation,                  // TRANSLATION
    Treat,                        // TREAT
    Trigger,                      // TRIGGER
    True,                         // TRUE
    Uescape,                      // UESCAPE
    Union,                        // UNION
    Unique,                       // UNIQUE
    Unknown,                      // UNKNOWN
    Unnest,                       // UNNEST
    Update,                       // UPDATE
    Upper,                        // UPPER
    User,                         // USER
    Using,                        // USING
    Value,                        // VALUE
    Values,                       // VALUES
    VarPop,                       // VAR_POP
    VarSamp,                      // VAR_SAMP
    Varchar,                      // VARCHAR
    Varying,                      // VARYING
    When,                         // WHEN
    Whenever,                     // WHENEVER
    Where,                        // WHERE
    WidthBucket,                  // WIDTH_BUCKET
    Window,                       // WINDOW
    With,                         // WITH
    Within,                       // WITHIN
    Without,                      // WITHOUT
    Year,                         // YEAR
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(C)]
pub struct Tokens<'a> {
    pub tok: &'a [Token],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a [Token]) -> Self {
        Tokens {
            tok: vec,
            start: 0,
            end: vec.len(),
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl InputLength for Token {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token;
    type Iter = Enumerate<::std::slice::Iter<'a, Token>>;
    type IterElem = ::std::slice::Iter<'a, Token>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Token>> {
        self.tok.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, Token> {
        self.tok.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }
}
