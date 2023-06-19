use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punct {
    Comma,               // ,
    Tilde,               // ~
    Plus,                // +
    Minus,               // -
    StringConcat,        // ||
    Star,                // *
    Slash,               // /
    Percent,             // %
    Ampersand,           // &
    Vertical,            // |
    ShiftLeft,           // <<
    ShiftRight,          // >>
    Lt,                  // <
    Gt,                  // >
    LtEq,                // <=
    GtEq,                // >=
    Eq,                  // =
    DoubleEq,            // ==
    NotEq,               // <>
    NotEq2,              // !=
    Period,              // .
    LParen,              // (
    RParen,              // )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Asc,
    All,
    As,
    And,
    By,
    Bool,
    Boolean,
    Between,
    Cast,
    Cross,
    Case,
    Distinct,
    Desc,
    Except,
    Exists,
    Escape,
    Else,
    End,
    First,
    Full,
    Filter,
    Float,
    From,
    False,
    Group,
    Having,
    Is,
    In,
    Int,
    Intersect,
    Integer,
    Inner,
    Join,
    Left,
    Limit,
    Like,
    Last,
    Nulls,
    Natural,
    Not,
    Null,
    Offset,
    On,
    Outer,
    Order,
    Or,
    Right,
    Select,
    String,
    Then,
    True,
    Using,
    Union,
    Where,
    When,
}

#[derive(Debug)]
pub struct NotKeyword;

impl FromStr for Keyword {
    type Err = NotKeyword;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "ASC" => Ok(Self::Asc),
            "ALL" => Ok(Self::All),
            "AS" => Ok(Self::As),
            "AND" => Ok(Self::And),
            "BY" => Ok(Self::By),
            "BOOL" => Ok(Self::Bool),
            "BOOLEAN" => Ok(Self::Boolean),
            "BETWEEN" => Ok(Self::Between),
            "CAST" => Ok(Self::Cast),
            "CROSS" => Ok(Self::Cross),
            "CASE" => Ok(Self::Case),
            "DISTINCT" => Ok(Self::Distinct),
            "DESC" => Ok(Self::Desc),
            "EXCEPT" => Ok(Self::Except),
            "EXISTS" => Ok(Self::Exists),
            "ESCAPE" => Ok(Self::Escape),
            "ELSE" => Ok(Self::Else),
            "END" => Ok(Self::End),
            "FIRST" => Ok(Self::First),
            "FULL" => Ok(Self::Full),
            "FILTER" => Ok(Self::Filter),
            "FLOAT" => Ok(Self::Float),
            "FROM" => Ok(Self::From),
            "FALSE" => Ok(Self::False),
            "GROUP" => Ok(Self::Group),
            "HAVING" => Ok(Self::Having),
            "IS" => Ok(Self::Is),
            "IN" => Ok(Self::In),
            "INT" => Ok(Self::Int),
            "INTERSECT" => Ok(Self::Intersect),
            "INTEGER" => Ok(Self::Integer),
            "INNER" => Ok(Self::Inner),
            "JOIN" => Ok(Self::Join),
            "LEFT" => Ok(Self::Left),
            "LIMIT" => Ok(Self::Limit),
            "LIKE" => Ok(Self::Like),
            "LAST" => Ok(Self::Last),
            "NULLS" => Ok(Self::Nulls),
            "NATURAL" => Ok(Self::Natural),
            "NOT" => Ok(Self::Not),
            "NULL" => Ok(Self::Null),
            "OFFSET" => Ok(Self::Offset),
            "ON" => Ok(Self::On),
            "OUTER" => Ok(Self::Outer),
            "ORDER" => Ok(Self::Order),
            "OR" => Ok(Self::Or),
            "RIGHT" => Ok(Self::Right),
            "SELECT" => Ok(Self::Select),
            "THEN" => Ok(Self::Then),
            "TRUE" => Ok(Self::True),
            "USING" => Ok(Self::Using),
            "UNION" => Ok(Self::Union),
            "WHERE" => Ok(Self::Where),
            "WHEN" => Ok(Self::When),
            "STRING" => Ok(Self::String),
            _ => Err(NotKeyword)
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Ident {
        value: String,
        quote: Option<char>
    },
    Punct(Punct),
    Number(String),
    String(String),
    Space,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    line: u64,
    column: u64,
}

impl Location {
    pub fn new() -> Self {
        Self {
            line: 1,
            column: 1,
        }
    }

    pub fn move_by_char(&mut self, ch: char) {
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        }else {
            self.column += 1;
        }
    }

    pub fn line(&self) -> u64 {
        self.line
    }

    pub fn column(&self) -> u64 {
        self.column
    }
}

#[derive(Debug, Clone, Eq)]
pub struct TokenWithLocation {
    pub token: Token,
    pub location: Location,
}

impl PartialEq for TokenWithLocation {
    fn eq(&self, other: &Self) -> bool {
        self.token.eq(&other.token)
    }
}

// impl fmt::Display for Keyword {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Keyword::Select => write!(f, "{}", "SELECT"),
//             Keyword::Natural => write!(f, "{}", "NATURAL"),
//             Keyword::Left => write!(f, "{}", "LEFT"),
//             Keyword::Right => write!(f, "{}", "RIGHT"),
//             Keyword::Full => write!(f, "{}", "FULL"),
//             Keyword::Inner => write!(f, "{}", "INNER"),
//             Keyword::Cross => write!(f, "{}", "CROSS"),
//             Keyword::Outer => write!(f, "{}", "Outer"),
//             Keyword::Join => write!(f, "{}", "JOIN"),
//             Keyword::On => write!(f, "{}", "ON"),
//             Keyword::Using => write!(f, "{}", "USING"),
//             Keyword::As => write!(f, "{}", "AS"),
//             Keyword::Distinct => write!(f, "{}", "DISTINCT"),
//             Keyword::All => write!(f, "{}", "ALL"),
//             Keyword::From => write!(f, "{}", "FROM"),
//             Keyword::Where => write!(f, "{}", "WHERE"),
//             Keyword::Group => write!(f, "{}", "GROUP"),
//             Keyword::By => write!(f, "{}", "BY"),
//             Keyword::Having => write!(f, "{}", "HAVING"),
//             Keyword::Order => write!(f, "{}", "ORDER"),
//             Keyword::Limit => write!(f, "{}", "LIMIT"),
//             Keyword::Asc => write!(f, "{}", "ASC"),
//             Keyword::Desc => write!(f, "{}", "DESC"),
//             Keyword::Nulls => write!(f, "{}", "NULLS"),
//             Keyword::First => write!(f, "{}", "FIRST"),
//             Keyword::Last => write!(f, "{}", "LAST"),
//             Keyword::Case => write!(f, "{}", "CASE"),
//             Keyword::When => write!(f, "{}", "WHEN"),
//             Keyword::Then => write!(f, "{}", "THEN"),
//             Keyword::Else => write!(f, "{}", "ELSE"),
//             Keyword::End => write!(f, "{}", "END"),
//         }
//     }
// }