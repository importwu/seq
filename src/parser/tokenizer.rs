use rtor::{
    Parser,
    Input,
    Error,
};

#[derive(Debug)]
pub struct ParseError(String);


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

#[derive(Debug)]
pub struct TokenizeError(String);

impl<I> Parser<I> for Keyword 
where I: Input<Token = TokenWithLocation>
{
    type Output = ();
    type Error = ParseError;

    fn parse(&mut self, mut input: I) -> Result<(Self::Output, I), Self::Error> {
        match input.next() {
            Some(TokenWithLocation {token: Token::Keyword(keyword), location: _ }) if *self == keyword => Ok(((), input)),
            Some(x) => Err(ParseError("expected".into())),
            None => Err(ParseError("end of input".into()))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenWithLocation {
    pub token: Token,
    pub location: Location,
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
    Last
}

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

use core::num;
use std::str::Chars;

pub struct Tokenizer<'a> {
    location: Location,
    chars: Chars<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(query: &'a str) -> Self {
        Self {
            location: Location::new(),
            chars: query.chars(),
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<TokenWithLocation>, TokenizeError> {
        let mut tokens = vec![];
        while let Some(token) = self.next_token() {
            tokens.push(TokenWithLocation {
                token: token?,
                location: self.location,
            })
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Option<Result<Token, TokenizeError>>{
        loop {
            self.consume_whitespace();
            match self.peek_char()? {
                ':' => return Some(self.tokenize_punct(Punct::Colon)),
                ',' => return Some(self.tokenize_punct(Punct::Comma)),
                '~' => return Some(self.tokenize_punct(Punct::Tilde)),
                '+' => return Some(self.tokenize_punct(Punct::Plus)),
                '-' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('-') => {
                            self.next_char();
                            self.consume_single_comment();
                            continue;
                        }
                        _ => return Some(Ok(Token::Punct(Punct::Minus))),
                    }
                }
                '|' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('|') => return Some(self.tokenize_punct(Punct::StringConcat)),
                        _ => return Some(Ok(Token::Punct(Punct::Vertical))),
                    }
                }
                '*' => return Some(self.tokenize_punct(Punct::Star)),
                '/' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('*') => {
                            self.next_char();
                            self.consume_multiple_comment();
                            continue;
                        }
                        _ => return Some(Ok(Token::Punct(Punct::Slash))),
                    }
                }
                '%' => return Some(self.tokenize_punct(Punct::Percent)),
                '&' => return Some(self.tokenize_punct(Punct::Ampersand)),
                '<' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('<') => return Some(self.tokenize_punct(Punct::Lt)),
                        Some('>') => return Some(self.tokenize_punct(Punct::NotEq)),
                        Some('=') => return Some(self.tokenize_punct(Punct::LtEq)),
                        _ => return Some(Ok(Token::Punct(Punct::Lt))),
                    }
                }
                '>' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('>') => return Some(self.tokenize_punct(Punct::ShiftRight)),
                        Some('=') => return Some(self.tokenize_punct(Punct::GtEq)),
                        _ => return Some(Ok(Token::Punct(Punct::Gt))),
                    }
                }
                '=' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('=') => return Some(self.tokenize_punct(Punct::DoubleEq)),
                        _ => return Some(Ok(Token::Punct(Punct::Eq)))
                    }
                }
                '!' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('=') => return Some(self.tokenize_punct(Punct::NotEq2)),
                        _ => return Some(Err(TokenizeError(format!("token error at line {}, column {}.", self.location.line(), self.location.column()))))
                    }
                }
                '.' => return Some(self.tokenize_number()),
                '(' => return Some(self.tokenize_punct(Punct::LParen)),
                ')' => return Some(self.tokenize_punct(Punct::RParen)),
                '0'..='9' => return Some(self.tokenize_number()), 
                ch => {
                    println!("{:?}", ch);
                    unreachable!()
                }
            }
        }
    }

    fn tokenize_number(&mut self) -> Result<Token, TokenizeError> {
        let s = self.chars.as_str();
        let mut len = 0;
        let ch = match self.next_char() {
            Some('0'..='9') => {
                len += 1;
                loop {
                    match self.peek_char() {
                        Some('0'..='9') => {
                            self.next_char();
                            len += 1;
                            continue;
                        } 
                        Some('.') => {
                            self.next_char();
                            len += 1;
                            break loop {
                                match self.peek_char() {
                                    Some('0'..='9') => {
                                        self.next_char();
                                        len += 1;
                                        continue;
                                    }
                                    Some(ch) => break Some(ch),
                                    None => break None,
                                }
                            }
                        }
                        Some(ch) => break Some(ch),
                        None => break None,
                    }
                }
            }
            Some('.') => {
                len += 1;
                if let Some('0'..='9') = self.next_char() {
                    len += 1;
                    loop {
                        match self.peek_char() {
                            Some('0'..='9') => {
                                self.next_char();
                                len += 1;
                                continue;
                            }
                            Some(ch) => break Some(ch),
                            None => break None
                        }
                    }
                } else {
                    return Err(TokenizeError("token error invalid number".into()))
                }
            }
            _ => return Err(TokenizeError("token error invalid number".into()))
        };

        match ch {
            Some('e' | 'E') => {
                self.next_char();
                len += 1;
                match self.peek_char() {
                    Some('+' | '-') => {
                        self.next_char();
                        len += 1;
                    }
                    Some('0'..='9') => (),
                    _ => return Err(TokenizeError("token error invalid number".into()))
                }

                if let Some('0'..='9') = self.next_char() {
                    len += 1;
                    loop {
                        match self.peek_char() {
                            Some('0'..='9') => {
                                self.next_char();
                                len += 1;
                                continue;
                            }
                            Some(ch) if ch.is_alphabetic() => return Err(TokenizeError("token error invalid number".into())),
                            _ => break
                        }
                    }

                } else {
                    return Err(TokenizeError("token error invalid number".into()))
                }
                
            },
            Some(ch) if ch.is_alphabetic() => return Err(TokenizeError("token error invalid number".into())),
            _ => ()
        }

        Ok(Token::Literal(Literal::Number(s[..len].into())))
    }


    #[inline]
    fn tokenize_punct(&mut self, punct: Punct) -> Result<Token, TokenizeError> {
        self.next_char();
        Ok(Token::Punct(punct))
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.chars.next()?;
        self.location.move_by_char(ch);
        Some(ch)
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.clone().next()
    }

    fn consume_single_comment(&mut self) {
        loop {
            match self.next_char() {
                Some('\n') => break,
                Some(_) => continue,
                None => break
            }
        }
    }

    fn consume_multiple_comment(&mut self) {
        loop {
            match self.next_char() {
                Some('*') => match self.next_char() {
                    Some('/') => break,
                    Some(_) => continue,
                    None => break,
                }
                Some(_) => continue,
                None => break,
            }
        }
    }

    fn consume_whitespace(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_whitespace() {
                self.next_char();
            }else {
                break;
            }
        }
    }
}



#[test]
fn test() {
    let tokenizer = Tokenizer::new("--fuck \n .12e+0+12");
    // let mut tokenizer = Tokenizer::new("abcde");
    let xs = tokenizer.tokenize();
    println!("{:?}", xs)
    // println!("{:?}", tokenizer.next_char());
    // println!("{:?}", tokenizer.next_char());
    // println!("{:?}", tokenizer.peek_char());
    // println!("{:?}", tokenizer.next_char());
    // println!("{:?}", tokenizer.next_char());
}