use rtor::{
    Parser,
    Input,
    AsChar,
    Error,
    ParseResult,
    combine::{
        opt,
        skip_many1,
        skip_many,
        sep_by1
    },
    primitive::{
        digit,
        hex,
        token
    }
};

mod keyword;

#[derive(Debug)]
pub enum LiteralValue {
    Number(String)
}

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Mul,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Plus,
    Minus
}

#[derive(Debug)]
pub enum Expr {
    LiteralValue(LiteralValue),
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOperator,
        right: Box<Expr>
    },
    UnaryOp {
        op: UnaryOperator,
        expr: Box<Expr>
    },
    Tuple(Vec<Expr>),
    IsNull(Box<Expr>)
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

fn binary_op<I>(input: I) -> ParseResult<(BinaryOperator, u8, u8), I> 
where I: Input<Token = char>
{
    '+'.map(|_| (BinaryOperator::Plus, 3, 4))
        .or('*'.map(|_| (BinaryOperator::Mul, 5, 6)))
        .parse(input)
}

fn unary_op<I>(input: I) -> ParseResult<(UnaryOperator, u8), I> 
where I: Input<Token = char>
{
    '+'.map(|_| (UnaryOperator::Plus, 7))
        .or('-'.map(|_| (UnaryOperator::Minus, 7)))
        .parse(input)
}

fn expr<I>(min_bp: u8) -> impl Parser<I, Output = Expr, Error = Error<I::Token>>
where I: Input<Token = char>
{
    move |input: I| {

        let (mut left, mut input) = loop {

            if let Ok((value, i)) = token(unsigned_numeric_literal).parse(input.clone()) {
                break (Expr::LiteralValue(value), i)
            }

            if let Ok(((op, bp), i)) = token(unary_op).parse(input.clone()) {
                let (expr, i) = expr(bp).parse(i)?;
                break (Expr::UnaryOp { op, expr: Box::new(expr) }, i)
            }

            if let Ok((_, i)) = token('(').parse(input.clone()) {
                let (expr, i) = sep_by1(expr(0), token(',')).parse(i)?;
                let (_, i) = token(')').parse(i)?;
                break (Expr::Tuple(expr), i)
            }
                
            return Err(Error::Unexpected('?'))
        };

        loop {

            if let Ok(((op, l_bp, r_bp), i)) = token(binary_op).parse(input.clone()) {
                if l_bp < min_bp {
                    break;
                }

                let (right, i) = expr(r_bp).parse(i)?;

                left = Expr::BinaryOp { 
                    left: Box::new(left), 
                    op, 
                    right: Box::new(right)
                };

                input = i;

                continue;
            }

            if let Ok(((_, l_bp), i)) = token("ISNULL").map(|o| (o, 1u8)).parse(input.clone()) {
                if l_bp < min_bp {
                    break;
                }

                left = Expr::IsNull(Box::new(left));
                input = i;
                continue;
            }

            break;
        }

        Ok((left, input))
    }
}


#[test]
fn test() {
    let r =expr(0).parse("( 1 + 2 , 3 + 4 ) * 5");
    println!("{:#?}", r)
}