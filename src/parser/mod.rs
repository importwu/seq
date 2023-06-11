mod tokenizer;
mod expr;
mod stmt;
mod ast;
mod keyword;
mod punct;

use rtor::{
    Parser,
    Input,
    Error,
};

use tokenizer::{
    Token,
    TokenWithLocation
};

use ast::{
    DataType,
    Ident,
    Literal
};


#[derive(Debug)]
pub struct ParseError(String);

type ParseResult<T, I> = Result<(T, I), ParseError>;

fn data_type<I>(mut input: I) -> ParseResult<DataType, I> 
where I: Input<Token = TokenWithLocation>
{
    match input.next() {
        Some(TokenWithLocation {token: Token::Ident(Ident {value, quote: None}), location}) => {
            match value.to_uppercase().as_str() {
                "INT" | "INTEGER" => Ok((DataType::Integer, input)),
                "FLOAT" => Ok((DataType::Float, input)),
                "BOOL" | "BOOLEAN" => Ok((DataType::Boolean, input)),
                "STRING" => Ok((DataType::String, input)),
                _ => return Err(ParseError(format!("{}, {}", location.line(), location.column()))),
            }   
        } 
        Some(TokenWithLocation { token, location }) => Err(ParseError(format!("{}, {}", location.line(), location.column()))),
        None => return Err(ParseError("end of input".into()))
    }
}


fn literal<I>(mut input: I) -> ParseResult<Literal, I>
where I: Input<Token = TokenWithLocation>
{
    match input.next() {
        Some(TokenWithLocation {token: Token::Literal(literal), location: _ }) => Ok((literal, input)),
        Some(TokenWithLocation { token, location }) => Err(ParseError(format!("{}, {}", location.line(), location.column()))),
        _ => Err(ParseError("end of input".into()))
    }
}

fn ident<I>(mut input: I) -> ParseResult<Ident, I>
where I: Input<Token = TokenWithLocation>
{
    match input.next() {
        Some(TokenWithLocation {token: Token::Ident(ident), location: _ }) => Ok((ident, input)),
        Some(TokenWithLocation { token, location }) => Err(ParseError(format!("{}, {}", location.line(), location.column()))),
        _ => Err(ParseError("end of input".into()))
    }
}