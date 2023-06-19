use std::str::FromStr;

use rtor::{
    Input,
    ParseResult,
    primitive::{
        ascii::digit,
        unicode, 
        eof,
        newline, 
        anychar, 
        pure,
    },
    combine::{
        skip_many,
        skip_many1,
        opt,
        recognize,
        not
    },
    Parser, 
    ParseError
};

use super::token::{
    Punct,
    Keyword,
    Token,
    Location,
    TokenWithLocation,
};

#[derive(Debug)]
pub struct TokenizeError(String);

#[derive(Debug, Clone)]
struct LocatedInput<I> {
    inner: I,
    location: Location
}

impl<I> LocatedInput<I> {
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

impl<I> Input for LocatedInput<I> 
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
        LocatedInput { 
            inner: self.inner.diff(&other.inner), 
            location: self.location
        }
    }

    fn tokens(&self) -> Self::Tokens {
        self.inner.tokens()
    }
}

pub fn tokenize<I>(query: I) ->  Result<Vec<TokenWithLocation>, TokenizeError> 
where I: Input<Token = char>
{   
    let mut tokens = vec![];

    let mut input = LocatedInput::new(query);

    let mut location = input.location();
    while let (Some(token), i) = next_token.map_err(|e| match e {
        ParseError::Eoi => TokenizeError(format!("token error: <eof> at line {}, column {}", location.line(), location.column())),
        ParseError::Unexpected(ch) => TokenizeError(format!("token error: unexpected char {} at line {}, column {}", ch, location.line(), location.column())),
        ParseError::Message(msg) => TokenizeError(format!("token error: {} at line {}, column {}", msg, location.line(), location.column()))
    }).parse(input)? {

        if token != Token::Space {
            tokens.push(TokenWithLocation {
                token,
                location,
            });
        }

        input = i;
        location = input.location();
    }

    Ok(tokens)
}

fn next_token<I>(mut input: I) -> ParseResult<Option<Token>, I> 
where I: Input<Token = char>
{
    match input.peek() {
        Some(ch) if ch.is_whitespace() => skip_many1(unicode::space).map(|_| Some(Token::Space)).parse(input),
        Some(',') => { input.next();  Ok((Some(Token::Punct(Punct::Comma)), input)) }
        Some('~') => { input.next();  Ok((Some(Token::Punct(Punct::Tilde)), input)) }
        Some('+') => { input.next();  Ok((Some(Token::Punct(Punct::Plus)), input)) }
        Some('*') => { input.next();  Ok((Some(Token::Punct(Punct::Star)), input)) }
        Some('%') => { input.next();  Ok((Some(Token::Punct(Punct::Percent)), input)) }
        Some('&') => { input.next();  Ok((Some(Token::Punct(Punct::Ampersand)), input)) }
        Some('(') => { input.next();  Ok((Some(Token::Punct(Punct::LParen)), input)) }
        Some(')') => { input.next();  Ok((Some(Token::Punct(Punct::RParen)), input)) }
        Some('<') => {
            input.next();
            '<'.map(|_| Some(Token::Punct(Punct::ShiftLeft)))
                .or('>'.map(|_| Some(Token::Punct(Punct::NotEq))))
                .or('='.map(|_| Some(Token::Punct(Punct::LtEq))))
                .or(pure(Some(Token::Punct(Punct::Lt))))
                .parse(input)
        }
        Some('>') => {
            input.next();
            '>'.map(|_| Some(Token::Punct(Punct::ShiftRight)))
                .or('='.map(|_| Some(Token::Punct(Punct::GtEq))))
                .or(pure(Some(Token::Punct(Punct::Gt))))
                .parse(input)
        }
        Some('=') => {
            input.next();
            '='.map(|_| Some(Token::Punct(Punct::DoubleEq)))
                .or(pure(Some(Token::Punct(Punct::Eq))))
                .parse(input)
        }
        Some('!') => {
            input.next();
            '='.map(|_| Some(Token::Punct(Punct::NotEq2))).parse(input)
        }
        Some('|') => {
            input.next();
            '|'.map(|_| Some(Token::Punct(Punct::StringConcat)))
                .or(pure(Some(Token::Punct(Punct::Vertical))))
                .parse(input)
        }
        Some('-') => {
            input.next();
            '-'.andr(skip_many(not(newline).andr(anychar)))
                .andr(eof.or(newline.ignore()))
                .map(|_| Some(Token::Space))
                .or(pure(Some(Token::Punct(Punct::Minus))))
                .parse(input)
        }
        Some('/') => {
            input.next();
            '*'.andr(skip_many(not("*/").andr(anychar)))
                .andr(eof.or("*/".ignore()))
                .map(|_| Some(Token::Space))
                .or(pure(Some(Token::Punct(Punct::Slash))))
                .parse(input)
        }
        Some('.' | '0'..='9') => {
            let fraction = '.'.andr(skip_many(digit));
            let fraction1 = '.'.andr(skip_many1(digit));
            let exponent = 'E'.or('e').andr(opt('+'.or('-'))).andr(skip_many1(digit));
            let number = recognize(skip_many1(digit).andl(opt(fraction)).or(fraction1).andl(opt(exponent)))
                .map(|i: I| Some(Token::Number(i.tokens().collect())));
            '.'.andr(not(digit)).map(|_| Some(Token::Punct(Punct::Period)))
                .or(number.andl(not(unicode::alpha)))
                .parse(input)
        }
        Some('\'') => {
            input.next();
            recognize(skip_many(not('\'').andr(anychar))).andl('\'')
                .map(|i: I| Some(Token::String(i.tokens().collect())))
                .parse(input)
        }
        Some(start_quote@('`' | '[' | '"')) => {
            input.next();
            let end_quote = match start_quote {
                '`' => '`',
                '[' => ']',
                '"' => '"',
                _ => unreachable!() 
            };

            recognize(skip_many(not((end_quote)).andr(anychar))).andl(end_quote)
                .map_err(|_| ParseError::Message("quote word".into()))
                .map(|i: I| Some(Token::Ident{ 
                    value: i.tokens().collect::<String>().trim().to_owned(), 
                    quote: Some(start_quote) 
                }))
                .parse(input)
        },
        Some(_) => {
            recognize(unicode::alpha.or('_').andr(skip_many(unicode::alpha.or('_').or('$').or(digit))))
            .map(|i: I| {
                let word = i.tokens().collect::<String>();
                Some(Keyword::from_str(&word)
                    .map(Token::Keyword)
                    .unwrap_or(Token::Ident { value: word, quote: None }))
            })
            .parse(input)
        },
        None => return Ok((None, input))
    }
}

#[test]
fn test() {
    let xs = tokenize("[ d da ");
    println!("{:?}", xs)

}