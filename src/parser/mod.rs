use rtor::{
    Parser,
    Input,
    AsChar,
    Error,
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

#[derive(Debug)]
pub enum LiteralValue {
    Number(String)
}

#[derive(Debug)]
pub enum BinOperator {
    Plus,
    Mul,
}

#[derive(Debug)]
pub enum Expr {
    LiteralValue(LiteralValue),
    BinOp {
        left: Box<Expr>,
        op: BinOperator,
        right: Box<Expr>
    }
}

fn peek<I, P>(mut parser: P) -> impl Parser<I, Output = P::Output, Error = P::Error> 
where 
    I: Input,
    P: Parser<I>
{
    move |input: I| {
        let (o, _) = parser.parse(input.clone())?;
        Ok((o, input))
    }
}

fn unsigned_numeric_literal<I>(input: I) -> ParseResult<LiteralValue, I> 
where I: Input<Token = char>
{
    let src = input.clone();

    let fraction = '.'.and(skip_many(digit));
    let fraction1 = '.'.and(skip_many1(digit));
    let exponent = 'E'.or('e').and(opt('+'.or('-'))).and(skip_many1(digit));

    let (_, i) = skip_many1(digit)
        .and(opt(fraction))
        .or(fraction1)
        .and(opt(exponent))
        .parse(input)?;

    let s = src.diff(&i).tokens().collect::<String>();

    Ok((LiteralValue::Number(s), i))
}

fn binop<I>(input: I) -> ParseResult<(BinOperator, u8, u8), I> 
where I: Input<Token = char>
{
    '+'.map(|_| (BinOperator::Plus, 1, 2))
        .or('*'.map(|_| (BinOperator::Mul, 3, 4)))
        .parse(input)
}

fn expr_binop<I>(min_bp: u8) -> impl Parser<I, Output = Expr, Error = Error<I::Token>>
where I: Input<Token = char>
{
    move |mut input: I| {
        let (value, i) = unsigned_numeric_literal.parse(input)?;

        let mut left = Expr::LiteralValue(value);

        input = i;

        loop {
            // let ((op, l_bp, r_bp), i) = peek(binop).parse(input)?;
            let ((op, l_bp, r_bp), _) = match binop.parse(input.clone()) {
                Ok(o) => o,
                Err(Error::Eoi) => break,
                Err(e) => return Err(e)
            };

            if l_bp < min_bp {
                break;
            }

            let (_, i) = binop.parse(input).unwrap();

            let (right, i) = expr_binop(r_bp).parse(i)?;


            left = Expr::BinOp { 
                left: Box::new(left), 
                op, 
                right: Box::new(right)
            };

            input = i;
        }

        Ok((left, input))
    }
}


#[test]
fn test() {
    let r =expr_binop(0).parse("1+2*3*4");
    println!("{:?}", r)
}