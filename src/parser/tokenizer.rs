use std::collections::HashMap;

use lazy_static::lazy_static;

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
        token, 
        peek, 
        not
    },
    Parser, 
    ParseError
};

use super::token::{
    Punct,
    Keyword,
    Ident,
    Literal,
    Token,
    Location,
    TokenWithLocation,
};

#[derive(Debug)]
pub struct TokenizeError(String);

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
            '.'.andr(not(digit)).map(|_| Some(Token::Punct(Punct::Period)))
                .or(number.andl(peek(not(unicode::alpha).or(eof))))
                .parse(input)
        }
        Some('\'') => {
            input.next();
            recognize(skip_many(not('\'').andr(anychar))).andl('\'')
                .map(|i: I| Some(Token::Literal(Literal::String(i.tokens().collect()))))
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

            let ident = recognize(skip_many(not((end_quote)).andr(anychar)))
                .map(|i: I| i.tokens().collect::<String>().trim().to_owned());

            ident
                .andl(end_quote)
                .map_err(|_| ParseError::Message("quote word".into()))
                .map(|value| Some(Token::Ident(Ident { value, quote: Some(start_quote) })))
                .parse(input)
        },
        Some(_) => {
            let (word, i) = word.parse(input)?;
            let word_uppercase = word.to_uppercase();
            match word_uppercase.as_str() {
                "FALSE" => Ok((Some(Token::Literal(Literal::Boolean(false))), i)),
                "TRUE" => Ok((Some(Token::Literal(Literal::Boolean(true))), i)),
                "NULL" => Ok((Some(Token::Literal(Literal::Null)), i)),
                _ => match KEYWORDS.get(word_uppercase.as_str()) 
                {
                    Some(&keyword) => Ok((Some(Token::Keyword(keyword)), i)),
                    _ => Ok((Some(Token::Ident(Ident { value: word, quote: None })), i))
                }
            }
        },
        None => return Ok((None, input))
    }
}

fn number<I>(input: I) -> ParseResult<Option<Token>, I> 
where I: Input<Token = char>
{
    let fraction = '.'.andr(skip_many(digit));
    let fraction1 = '.'.andr(skip_many1(digit));
    let exponent = 'E'.or('e')
        .andr(opt('+'.or('-')))
        .andr(skip_many1(digit));
    let number = skip_many1(digit)
        .andl(opt(fraction))
        .or(fraction1)
        .andl(opt(exponent));
    recognize(number).map(|i: I| Some(Token::Literal(Literal::Number(i.tokens().collect())))).parse(input)
}

fn word<I>(input: I) -> ParseResult<String, I> 
where I: Input<Token = char>
{
    recognize(
        unicode::alpha
        .or('_')
        .andr(skip_many(unicode::alpha.or('_').or('$').or(digit)))
    )
    .map(|i: I| i.tokens().collect())
    .parse(input)
}

#[test]
fn test() {
    let xs = tokenize("[ 1 2 ");
    println!("{:?}", xs)

}