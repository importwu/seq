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
    Select,
    Natural,
    Left,
    Right,
    Full,
    Inner,
    Cross,
    Outer,
    Join,
    On,
    Using,
    As,
    Distinct,
    All,
    From,
    Where,
    Group,
    By,
    Having,
    Order,
    Limit,
    Asc,
    Desc,
    Nulls,
    First,
    Last,
    Case,
    When,
    Then,
    Else,
    End,
    And,
    Or,
    Not,
    Cast,
    Is,
    Between,
    Like,
    Escape,
    IsNull,
    NotNull,
    Null,
    Collate,
    Filter,
    Union,
    Intersect,
    Except,
    Exists,
    Offset,
    In,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Number(String),
    Boolean(bool),
    String(String),
    Null
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub value: String,
    pub quote: Option<char>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
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