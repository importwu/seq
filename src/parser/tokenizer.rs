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

use std::str::Chars;
use std::iter::Peekable;

pub struct Tokenizer<'a> {
    location: Location,
    peekable: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(query: &'a str) -> Self {
        Self {
            location: Location::new(),
            peekable: query.chars().peekable()
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<TokenWithLocation>, TokenizeError> {
        let mut tokens = vec![];
        while let Some(token) = self.next_token() {
            tokens.push(token?)
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Option<Result<TokenWithLocation, TokenizeError>>{
        loop {
            self.consume_whitespace();
            match self.next_char()? {
                ':' => return Some(Ok(self.make_punct(Punct::Colon))),
                ',' => return Some(Ok(self.make_punct(Punct::Comma))),
                '~' => return Some(Ok(self.make_punct(Punct::Tilde))),
                '+' => return Some(Ok(self.make_punct(Punct::Plus))),
                '-' => match self.peek_char() {
                    Some('-') => {
                        self.next_char();
                        self.consume_single_comment();
                        continue;
                    }
                    _ => return Some(Ok(self.make_punct(Punct::Minus))),
                }
                '|' => match self.peek_char() {
                    Some('|') => {
                        self.next_char();
                        return Some(Ok(self.make_punct(Punct::StringConcat)))
                    }
                    _ => return Some(Ok(self.make_punct(Punct::Vertical))),
                }
                '*' => return Some(Ok(self.make_punct(Punct::Star))),
                '/' => match self.peek_char() {
                    Some('*') => {
                        self.next_char();
                        self.consume_multiple_comment();
                        continue;
                    }
                    _ => return Some(Ok(self.make_punct(Punct::Slash))),
                }
                '%' => return Some(Ok(self.make_punct(Punct::Percent))),
                '&' => return Some(Ok(self.make_punct(Punct::Ampersand))),
                '<' => match self.peek_char() {
                    Some('<') => {
                        self.next_char();
                        return Some(Ok(self.make_punct(Punct::ShiftLeft)))
                    }
                    Some('>') => {
                        self.next_char();
                        return Some(Ok(self.make_punct(Punct::NotEq)))
                    }
                    Some('=') => {
                        self.next_char();
                        return Some(Ok(self.make_punct(Punct::LtEq)))
                    }
                    _ => return Some(Ok(self.make_punct(Punct::Lt))),
                }
                '>' => match self.peek_char() {
                    Some('>') => {
                        self.next_char();
                        return Some(Ok(self.make_punct(Punct::ShiftRight)))
                    }
                    Some('=') => {
                        self.next_char();
                        return Some(Ok(self.make_punct(Punct::GtEq)))
                    }
                    _ => return Some(Ok(self.make_punct(Punct::Gt))),
                }
                '=' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        return Some(Ok(self.make_punct(Punct::DoubleEq)))
                    }
                    _ => return Some(Ok(self.make_punct(Punct::Eq)))
                }
                '!' => match self.next_char() {
                    Some('=') => return Some(Ok(self.make_punct(Punct::NotEq2))),
                    _ => return Some(Err(TokenizeError(format!("token error at line {}, column {}.", self.location.line(), self.location.column()))))
                }
                '.' => match self.peek_char() {
                    Some(&ch@'0'..='9') => return Some(self.consume_and_make_number(ch)),
                    _ => return Some(Ok(self.make_punct(Punct::Period))),
                }
                '(' => return Some(Ok(self.make_punct(Punct::LParen))),
                ')' => return Some(Ok(self.make_punct(Punct::RParen))),
                ch@'0'..='9' => return Some(self.consume_and_make_number(ch)), 
                ch => {
                    println!("{:?}", ch);
                    unreachable!()
                }
            }
        }
    }

    fn consume_and_make_number(&mut self, ch: char) -> Result<TokenWithLocation, TokenizeError> {
        let mut number = String::new();
        number.push(ch);
        let ch = match ch {
            '0'..='9' => {
                loop {
                    match self.peek_char() {
                        Some(&ch@'0'..='9') => {
                            self.next_char();
                            number.push(ch);
                            continue;
                        } 
                        Some('.') => {
                            self.next_char();
                            number.push('.');
                            break loop {
                                match self.peek_char() {
                                    Some(&ch@'0'..='9') => {
                                        self.next_char();
                                        number.push(ch);
                                        continue;
                                    }
                                    Some(&ch) => break Some(ch),
                                    None => break None,
                                }
                            }
                        }
                        Some(&ch) => break Some(ch),
                        None => break None,
                    }
                }
            }
            '.' => {
                if let Some(ch@'0'..='9') = self.next_char() {
                    number.push(ch);
                    loop {
                        match self.peek_char() {
                            Some(&ch@'0'..='9') => {
                                self.next_char();
                                number.push(ch);
                                continue;
                            }
                            Some(&ch) => break Some(ch),
                            None => break None
                        }
                    }
                }else {
                    return Err(TokenizeError("token error invalid number".into()))
                }
            }
            _ => unreachable!()
        };

        match ch {
            Some('e' | 'E') => {

            },
            Some(ch) if ch.is_whitespace() | ch.is_ascii_punctuation() |
            None => return Ok(TokenWithLocation { token: Token::Literal(Literal::Number(number)), location: self.location }),
            Some(ch) if ch.is_alphabetic() => return Err(TokenizeError("token error invalid number".into()))
        }

        todo!()
    }

    #[inline]
    fn make_punct(&self, punct: Punct) -> TokenWithLocation {
        TokenWithLocation {
            token: Token::Punct(punct),
            location: self.location
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let ch =  self.peekable.next()?;
        self.location.move_by_char(ch);
        Some(ch)
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.peekable.peek()
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
        println!("comment");
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
        while let Some(&ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.next_char();
            }else {
                break;
            }
        }
    }
}



#[test]
fn test() {
    let mut tokenizer = Tokenizer::new("--fuck \n ,");
    let xs = tokenizer.tokenize();
    println!("{:?}", xs);
}