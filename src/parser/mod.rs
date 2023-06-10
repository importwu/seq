mod tokenizer;
mod expr;
mod select;

use std::{collections::HashMap};
use lazy_static::lazy_static;
use rtor::{
    Parser,
    Input,
    Error, primitive::eof,
};

use tokenizer::{
    Token,
    TokenWithLocation
};

use self::{select::{Select}, tokenizer::tokenize};

#[derive(Debug)]
pub struct ParseError(String);

type ParseResult<T, I> = Result<(T, I), ParseError>;

#[derive(Debug, Clone, Copy)]
pub enum DataType {
    Integer,
    Float,
    String,
    Boolean,
}

fn data_type<I>(mut input: I) -> ParseResult<DataType, I> 
where I: Input<Token = TokenWithLocation>
{
    match input.next() {
        Some(TokenWithLocation {token: Token::Ident(Ident {value, quote: None}), location: _}) => {
            match value.to_uppercase().as_str() {
                "INT" | "INTEGER" => Ok((DataType::Integer, input)),
                "FLOAT" => Ok((DataType::Float, input)),
                "BOOL" | "BOOLEAN" => Ok((DataType::Boolean, input)),
                "STRING" => Ok((DataType::String, input)),
                _ => return Err(ParseError("invalid datatype".into())),
            }   
        } 
        Some(TokenWithLocation {token, location}) => return Err(ParseError("invalid datatype".into())), 
        None => return Err(ParseError("end of input".into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Number(String),
    Boolean(bool),
    String(String)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub value: String,
    pub quote: Option<char>
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
    Offset
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

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Keyword> = {
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
        keywords
    };
}

impl<I> Parser<I> for Keyword 
where I: Input<Token = TokenWithLocation>
{
    type Output = ();
    type Error = ParseError;

    fn parse(&mut self, mut input: I) -> Result<(Self::Output, I), Self::Error> {
        match input.next() {
            Some(TokenWithLocation {token: Token::Keyword(keyword), location: _ }) if *self == keyword => Ok(((), input)),
            Some(TokenWithLocation { token, location }) => Err(ParseError(format!("{}, {}, {}", token, location.line(), location.column()))),
            None => Err(ParseError("end of input".into()))
        }
    }
}

impl<I> Parser<I> for Punct 
where I: Input<Token = TokenWithLocation>
{
    type Output = ();
    type Error = ParseError;

    fn parse(&mut self, mut input: I) -> Result<(Self::Output, I), Self::Error> {
        match input.next() {
            Some(TokenWithLocation {token: Token::Punct(punct), location: _ }) if *self == punct => Ok(((), input)),
            Some(TokenWithLocation { token, location }) => Err(ParseError(format!("{}, {}, {}", token, location.line(), location.column()))),
            None => Err(ParseError("end of input".into()))
        }
    }
}

fn ident<I>(mut input: I) -> ParseResult<Ident, I>
where I: Input<Token = TokenWithLocation>
{
    match input.next() {
        Some(TokenWithLocation {token: Token::Ident(ident), location: _ }) => Ok((ident, input)),
        _ => Err(ParseError("end of input".into()))
    }
}

fn literal<I>(mut input: I) -> ParseResult<Literal, I>
where I: Input<Token = TokenWithLocation>
{
    match input.next() {
        Some(TokenWithLocation {token: Token::Literal(literal), location: _ }) => Ok((literal, input)),
        _ => Err(ParseError("end of input".into()))
    }
}

// fn parse<I>(query: I) -> Result<Stmt, ParseError> 
// where I: Input<Token = char>
// {
//     let tokens = tokenize(query).unwrap();
//     stmt.parse(tokens.as_slice());
   
//     todo!()
// }