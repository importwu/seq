use std::str::Chars;

use rtor::{
    Input,
    ParseResult,
    primitive::{
        space
    },
    combine::{
        skip_many
    }, Parser, Error
};

use super::{
    Keyword,
    Ident,
    Punct,
    Literal,
    KEYWORDS,
};

#[derive(Debug)]
pub struct TokenizeError(String);

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

#[derive(Debug, Clone)]
pub struct InputWithLocation<I> {
    inner: I,
    location: Location
}

impl<I> InputWithLocation<I> {
    pub fn new(inner: I) -> Self {
        Self {
            inner,
            location: Location::new()
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }
}


impl<I> Input for InputWithLocation<I> 
where 
    I: Input<Token = char>,
{
    type Token = I::Token;
    type Tokens = I::Tokens;

    fn next(&mut self) -> Option<Self::Token> {
        let ch = self.inner.next()?;
        self.location.move_by_char(ch);
        Some(ch)
    }

    fn peek(&mut self) -> Option<Self::Token> {
        self.inner.peek()
    }

    fn diff(&self, other: &Self) -> Self {
        InputWithLocation { 
            inner: self.inner.diff(&other.inner), 
            location: self.location
        }
    }

    fn tokens(&self) -> Self::Tokens {
        self.inner.tokens()
    }
}


#[test]

fn test_input() {
    let mut src = InputWithLocation::new("abcdefg");

    let c = src.clone();

    println!("{:?}", src.next());
    println!("{:?}", src.next());
    println!("{:?}", src.next());

    println!("{:?}", c.diff(&src))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenWithLocation {
    pub token: Token,
    pub location: Location,
}

// pub struct Tokenizer<'a> {
//     location: Location,
//     chars: Chars<'a>,
// }

// impl<'a> Tokenizer<'a> {
//     pub fn new(query: &'a str) -> Self {
//         Self {
//             location: Location::new(),
//             chars: query.chars(),
//         }
//     }

//     pub fn tokenize(mut self) -> Result<Vec<TokenWithLocation>, TokenizeError> {
//         let mut tokens = vec![];
//         while let Some(token) = self.next_token() {
//             tokens.push(TokenWithLocation {
//                 token: token?,
//                 location: self.location,
//             });
//         }
//         Ok(tokens)
//     }

//     fn next_token(&mut self) -> Option<Result<Token, TokenizeError>>{
//         loop {
//             self.consume_whitespace();
//             match self.peek_char()? {
//                 ':' => return Some(self.tokenize_punct(Punct::Colon)),
//                 ',' => return Some(self.tokenize_punct(Punct::Comma)),
//                 '~' => return Some(self.tokenize_punct(Punct::Tilde)),
//                 '+' => return Some(self.tokenize_punct(Punct::Plus)),
//                 '-' => {
//                     self.next_char();
//                     match self.peek_char() {
//                         Some('-') => {
//                             self.next_char();
//                             self.consume_single_comment();
//                             continue;
//                         }
//                         _ => return Some(Ok(Token::Punct(Punct::Minus))),
//                     }
//                 }
//                 '|' => {
//                     self.next_char();
//                     match self.peek_char() {
//                         Some('|') => return Some(self.tokenize_punct(Punct::StringConcat)),
//                         _ => return Some(Ok(Token::Punct(Punct::Vertical))),
//                     }
//                 }
//                 '*' => return Some(self.tokenize_punct(Punct::Star)),
//                 '/' => {
//                     self.next_char();
//                     match self.peek_char() {
//                         Some('*') => {
//                             self.next_char();
//                             self.consume_multiple_comment();
//                             continue;
//                         }
//                         _ => return Some(Ok(Token::Punct(Punct::Slash))),
//                     }
//                 }
//                 '%' => return Some(self.tokenize_punct(Punct::Percent)),
//                 '&' => return Some(self.tokenize_punct(Punct::Ampersand)),
//                 '<' => {
//                     self.next_char();
//                     match self.peek_char() {
//                         Some('<') => return Some(self.tokenize_punct(Punct::ShiftLeft)),
//                         Some('>') => return Some(self.tokenize_punct(Punct::NotEq)),
//                         Some('=') => return Some(self.tokenize_punct(Punct::LtEq)),
//                         _ => return Some(Ok(Token::Punct(Punct::Lt))),
//                     }
//                 }
//                 '>' => {
//                     self.next_char();
//                     match self.peek_char() {
//                         Some('>') => return Some(self.tokenize_punct(Punct::ShiftRight)),
//                         Some('=') => return Some(self.tokenize_punct(Punct::GtEq)),
//                         _ => return Some(Ok(Token::Punct(Punct::Gt))),
//                     }
//                 }
//                 '=' => {
//                     self.next_char();
//                     match self.peek_char() {
//                         Some('=') => return Some(self.tokenize_punct(Punct::DoubleEq)),
//                         _ => return Some(Ok(Token::Punct(Punct::Eq)))
//                     }
//                 }
//                 '!' => {
//                     self.next_char();
//                     match self.peek_char() {
//                         Some('=') => return Some(self.tokenize_punct(Punct::NotEq2)),
//                         _ => return Some(Err(self.token_error("invalid punct")))
//                     }
//                 }
//                 '(' => return Some(self.tokenize_punct(Punct::LParen)),
//                 ')' => return Some(self.tokenize_punct(Punct::RParen)),
//                 '.' | '0'..='9' => return Some(self.tokenize_number_or_period()),
//                 '\'' => return Some(self.tokenize_string()),
//                 _ => return Some(self.tokenize_keyword_or_ident_or_bool())
//             }
//         }
//     }

//     fn tokenize_keyword_or_ident_or_bool(&mut self) -> Result<Token, TokenizeError> {
//         match unsafe { self.peek_char().unwrap_unchecked() } {
//             ch@('"' | '`' | '[') => {
//                 self.next_char();
//                 self.consume_whitespace();
//                 let word = String::from(self.extract_word()?);
//                 self.consume_whitespace();
//                 let end_quote = match ch {
//                     '"' => '"',
//                     '`' => '`',
//                     '[' => ']',
//                     _ => unreachable!()
//                 };
//                 match self.next_char() {
//                     Some(end_quote) => Ok(Token::Ident(Ident {
//                         value: word,
//                         quote: Some(ch),
//                     })),
//                     _ => Err(self.token_error("invalid ident"))
//                 }
//             }  
//             _ => {
//                 let word = self.extract_word()?;
//                 let word_uppercase = word.to_uppercase();
//                 if word_uppercase == "TRUE"{
//                     return Ok(Token::Literal(Literal::Boolean(true)));
//                 }
//                 if word_uppercase == "FALSE" {
//                     return Ok(Token::Literal(Literal::Boolean(false)));
//                 }
//                 match KEYWORDS.get(word_uppercase.as_str()) {
//                     Some(&keyword) => Ok(Token::Keyword(keyword)),
//                     _ => Ok(Token::Ident(Ident {
//                         value: word.into(),
//                         quote: None
//                     }))
//                 }
//             }
//         }
//     }

//     fn extract_word(&mut self) -> Result<&str, TokenizeError> {
//         let s = self.chars.as_str();
//         let mut len = 0;
//         match self.next_char() {
//             Some(ch) if ch.is_alphabetic() | ch.eq(&'_')  => {
//                 len += ch.len_utf8();
//                 loop {
//                     match self.peek_char() {
//                         Some(ch) if ch.is_alphabetic() | ch.is_ascii_digit() | ch.eq(&'_') | ch.eq(&'$') => {
//                             self.next_char();
//                             len += ch.len_utf8();
//                             continue;
//                         }
//                         _ => break,
//                     }
//                 }
//             }
//             _ => return Err(self.token_error("invalid token"))
//         }
//         Ok(&s[..len])
//     }

//     fn tokenize_string(&mut self) -> Result<Token, TokenizeError> {
//         self.next_char();
//         let mut len = 0;
//         let s = self.chars.as_str();
//         loop {
//             match self.next_char() {
//                 Some('\'') => break,
//                 Some(ch) => {
//                     len += ch.len_utf8();
//                     continue;
//                 },
//                 None => return Err(self.token_error("invalid string"))
//             }
//         }
//         Ok(Token::Literal(Literal::String(s[..len].into())))
//     }

//     fn tokenize_number_or_period(&mut self) -> Result<Token, TokenizeError> {
//         let s = self.chars.as_str();
//         let mut len = 0;
//         let ch = match self.next_char() {
//             Some('0'..='9') => {
//                 len += 1;
//                 loop {
//                     match self.peek_char() {
//                         Some('0'..='9') => {
//                             self.next_char();
//                             len += 1;
//                             continue;
//                         } 
//                         Some('.') => {
//                             self.next_char();
//                             len += 1;
//                             break loop {
//                                 match self.peek_char() {
//                                     Some('0'..='9') => {
//                                         self.next_char();
//                                         len += 1;
//                                         continue;
//                                     }
//                                     ch => break ch,
//                                 }
//                             }
//                         }
//                         ch => break ch,
//                     }
//                 }
//             }
//             Some('.') => {
//                 len += 1;
//                 if let Some('0'..='9') = self.next_char() {
//                     len += 1;
//                     loop {
//                         match self.peek_char() {
//                             Some('0'..='9') => {
//                                 self.next_char();
//                                 len += 1;
//                                 continue;
//                             }
//                             ch => break ch,
//                         }
//                     }
//                 } else {
//                     return Ok(Token::Punct(Punct::Period))
//                 }
//             }
//             _ => unreachable!()
//         };

//         match ch {
//             Some('e' | 'E') => {
//                 self.next_char();
//                 len += 1;
//                 match self.peek_char() {
//                     Some('+' | '-') => {
//                         self.next_char();
//                         len += 1;
//                     }
//                     Some('0'..='9') => (),
//                     _ => return Err(self.token_error("invalid number"))
//                 }

//                 if let Some('0'..='9') = self.next_char() {
//                     len += 1;
//                     loop {
//                         match self.peek_char() {
//                             Some('0'..='9') => {
//                                 self.next_char();
//                                 len += 1;
//                                 continue;
//                             }
//                             Some(ch) if ch.is_alphabetic() => return Err(self.token_error("invalid number")),
//                             _ => break
//                         }
//                     }

//                 } else {
//                     return Err(self.token_error("invalid number"))
//                 }
                
//             },
//             Some(ch) if ch.is_alphabetic() => return Err(self.token_error("invalid number")),
//             _ => ()
//         }

//         Ok(Token::Literal(Literal::Number(s[..len].into())))
//     }

//     #[inline]
//     fn tokenize_punct(&mut self, punct: Punct) -> Result<Token, TokenizeError> {
//         self.next_char();
//         Ok(Token::Punct(punct))
//     }

//     fn next_char(&mut self) -> Option<char> {
//         let ch = self.chars.next()?;
//         self.location.move_by_char(ch);
//         Some(ch)
//     }

//     fn peek_char(&mut self) -> Option<char> {
//         self.chars.clone().next()
//     }

//     fn consume_single_comment(&mut self) {
//         loop {
//             match self.next_char() {
//                 Some('\n') => break,
//                 Some(_) => continue,
//                 None => break
//             }
//         }
//     }

//     fn consume_multiple_comment(&mut self) {
//         loop {
//             match self.next_char() {
//                 Some('*') => match self.next_char() {
//                     Some('/') => break,
//                     Some(_) => continue,
//                     None => break,
//                 }
//                 Some(_) => continue,
//                 None => break,
//             }
//         }
//     }

//     fn consume_whitespace(&mut self) {
//         while let Some(ch) = self.peek_char() {
//             if ch.is_ascii_whitespace() {
//                 self.next_char();
//             }else {
//                 break;
//             }
//         }
//     }

//     #[inline]
//     fn token_error(&self, msg: &str) -> TokenizeError {
//         TokenizeError(format!("{} at line {}, column {}.", msg, self.location.line(), self.location.column()))
//     }
// }

pub struct Tokenizer<I> {
    input: InputWithLocation<I>
}

impl<I> Tokenizer<I> 
where
    I: Input<Token = char>
{
    pub fn new(query: I) -> Self {
        Self { 
            input: InputWithLocation::new(query)
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<TokenWithLocation>, TokenizeError> {
        let mut tokens = vec![];

        let mut location = self.input.location();

        while let Some(token) = self.next_token() {
            tokens.push(TokenWithLocation {
                token: token?,
                location,
            });
            location = self.input.location();
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Option<Result<Token, TokenizeError>>{
        todo!()
    }


}

type TokenizeResult<O, I> = Result<(O, I), TokenizeError>;

pub fn tokenize<I>(query: I) ->  Result<Vec<TokenWithLocation>, TokenizeError> 
where I: Input<Token = char>
{   
    let mut tokens = vec![];

    let mut input = InputWithLocation::new(query);

    let mut location = input.location();

    while let (Some(token), i) = next_token.map_err(|_| TokenizeError("as".into())).parse(input)? {
        tokens.push(TokenWithLocation {
            token,
            location,
        });
        input = i;
        location = input.location();
    }

    Ok(tokens)
}

fn next_token<I>(input: I) -> ParseResult<Option<Token>, I> 
where I: Input<Token = char>
{
    loop {
        let (_, mut i) = skip_many(space).parse(input)?;
        match i.peek() {
            Some(':') => {
                i.next();
                return Ok((Some(Token::Punct(Punct::Colon)), i))
            },
            Some(_) => unimplemented!(),
            None => return Ok((None, i))
        }
    }
}



#[test]
fn test() {
    // let tokenizer = Tokenizer::new("--fuck \n 'fu我操' select * from student ");
    // let xs = tokenizer.tokenize();
    let xs = tokenize(": : : ");
    println!("{:?}", xs)
    // println!("{:?}", tokenizer.next_char());
    // println!("{:?}", tokenizer.next_char());
    // println!("{:?}", tokenizer.peek_char());
    // println!("{:?}", tokenizer.next_char());
    // println!("{:?}", tokenizer.next_char());
}