use rtor::{
    Parser,
    Input
};

use super::{
    ParseError,
    tokenizer::{
        TokenWithLocation,
        Token
    }
};



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


impl<I> Parser<I> for Punct 
where I: Input<Token = TokenWithLocation>
{
    type Output = ();
    type Error = ParseError;
    
    fn parse(&mut self, mut input: I) -> Result<(Self::Output, I), Self::Error> {
        match input.next() {
            Some(TokenWithLocation {token: Token::Punct(punct), location: _ }) if *self == punct => Ok(((), input)),
            Some(TokenWithLocation { token, location }) => Err(ParseError(format!("{}, {}", location.line(), location.column()))),
            None => Err(ParseError("end of input".into()))
        }
    }
}