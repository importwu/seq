use rtor::{
    Parser,
    Input,
    AsChar,
    ParseResult,
    combine::{
        opt,
        skip_many1,
        skip_many
    },
    primitive::{
        digit,
        hex
    }
};

mod keyword;

fn numeric_literal<I>(input: I) -> ParseResult<I, I> 
    where I: Input<Token = char>
{
    let src = input.clone();

    let fraction = '.'.and(skip_many(digit));
    let fraction1 = '.'.and(skip_many1(digit));
    let exponent = 'E'.or('e').and(opt('+'.or('-'))).and(skip_many1(digit));
    let hexdigit = "0x".or("0X").and(skip_many1(hex));

    let (_, i) = hexdigit
        .or(
    skip_many1(digit)
                .and(opt(fraction))
                .or(fraction1)
                .and(opt(exponent))
        )
        .parse(input)?;

    Ok((src.diff(&i),i))
}

#[test]
fn test() {
    let (s, _) = numeric_literal.parse("012e2").unwrap();
    println!("{:?}", s.parse::<f32>())
}