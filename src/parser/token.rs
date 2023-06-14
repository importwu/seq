use std::collections::HashMap;

use lazy_static::lazy_static;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Punct {
    Colon,               // :
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenWithLocation {
    pub token: Token,
    pub location: Location,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, Keyword> = {
        let mut keywords = HashMap::new();
        keywords.insert("SELECT", Keyword::Select);
        keywords.insert("NATURAL", Keyword::Natural);
        keywords.insert("LEFT", Keyword::Left);
        keywords.insert("RIGHT", Keyword::Right);
        keywords.insert("FULL", Keyword::Full);
        keywords.insert("INNER", Keyword::Inner);
        keywords.insert("CROSS", Keyword::Cross);
        keywords.insert("OUTER", Keyword::Outer);
        keywords.insert("JOIN", Keyword::Join);
        keywords.insert("ON", Keyword::On);
        keywords.insert("USING", Keyword::Using);
        keywords.insert("AS", Keyword::As);
        keywords.insert("DISTINCT", Keyword::Distinct);
        keywords.insert("ALL", Keyword::All);
        keywords.insert("FROM", Keyword::From);
        keywords.insert("WHERE", Keyword::Where);
        keywords.insert("GROUP", Keyword::Group);
        keywords.insert("BY", Keyword::By);
        keywords.insert("HAVING", Keyword::Having);
        keywords.insert("ORDER", Keyword::Order);
        keywords.insert("LIMIT", Keyword::Limit);
        keywords.insert("CASE", Keyword::Case);
        keywords.insert("WHEN", Keyword::When);
        keywords.insert("THEN", Keyword::Then);
        keywords.insert("ELSE", Keyword::Else);
        keywords.insert("END", Keyword::End);
        keywords.insert("AND", Keyword::And);
        keywords.insert("OR", Keyword::Or);
        keywords.insert("NOT", Keyword::Not);
        keywords.insert("CAST", Keyword::Not);
        keywords.insert("IS", Keyword::Is);
        keywords.insert("BETWEEN", Keyword::Between);
        keywords.insert("LIKE", Keyword::Like);
        keywords.insert("ESCAPE", Keyword::Escape);
        keywords.insert("ISNULL", Keyword::IsNull);
        keywords.insert("NOTNULL", Keyword::NotNull);
        keywords.insert("NULL", Keyword::Null);
        keywords.insert("COLLATE", Keyword::Collate);
        keywords.insert("FILTER", Keyword::Filter);
        keywords.insert("UNION", Keyword::Union);
        keywords.insert("Intersect", Keyword::Intersect);
        keywords.insert("EXCEPT", Keyword::Except);
        keywords.insert("EXISTS", Keyword::Exists);
        keywords.insert("OFFSET", Keyword::Offset);
        keywords.insert("IN", Keyword::In);
        keywords
    };
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