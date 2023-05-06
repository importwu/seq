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
        sep_by1,
        option, 
        between,
        recognize
    },
    primitive::{
        digit,
        hex,
        token,
        error
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
    Between {
        is_not: bool,
        expr: Box<Expr>,
        left: Box<Expr>,
        right: Box<Expr>
    },
    Tuple(Vec<Expr>),
    IsNull(Box<Expr>),
    NotNull(Box<Expr>),
    IsDistinctFrom(Box<Expr>, Box<Expr>),
    IsNotDistinctFrom(Box<Expr>, Box<Expr>),
    Case {
        operand: Option<Box<Expr>>,
        when_then: Vec<(Expr, Expr)>,
        else_cause: Option<Box<Expr>>,
    }
}

fn unsigned_numeric_literal<I>(input: I) -> ParseResult<LiteralValue, I> 
where I: Input<Token = char>
{
    let fraction = '.'.and(skip_many(digit));
    let fraction1 = '.'.and(skip_many1(digit));
    let exponent = 'E'.or('e').and(opt('+'.or('-'))).and(skip_many1(digit));

    let unsigned_numeric = skip_many1(digit)
        .and(opt(fraction))
        .or(fraction1)
        .and(opt(exponent));

    let (o, i) = recognize(unsigned_numeric).parse(input)?;
    let s = o.tokens().collect::<String>();

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

fn opt_not<I>(input: I) -> ParseResult<bool, I> 
where I: Input<Token = char>
{
    option(token("NOT"))
        .map(|x| x.map_or_else(||false, |_|true))
        .parse(input) 
}

fn expr_tuple<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = char>
{
    between(
        token('('), 
        sep_by1(expr(0), token(',')), 
        token(')')
    )
    .map(Expr::Tuple)
    .parse(input)
}

fn expr<I>(min: u8) -> impl Parser<I, Output = Expr, Error = Error<I::Token>>
where I: Input<Token = char>
{
    move |input: I| {

        let (mut lhs, mut input) = loop {

            if let Ok((value, i)) = token(unsigned_numeric_literal).parse(input.clone()) {
                break (Expr::LiteralValue(value), i)
            }

            if let Ok(((op, l), i)) = token(unary_op).parse(input.clone()) {
                let (expr, i) = expr(l).parse(i)?;
                break (Expr::UnaryOp { op, expr: Box::new(expr) }, i)
            }

            if let Ok(o) = expr_tuple.parse(input.clone()) {
                break o;
            }

            if let Ok((_, i)) = token("CASE").parse(input.clone()) {
                let (operand, i) = option(expr(0)).parse(i)?;
                let
            }

            return unsafe {Err(error.parse(input).unwrap_err_unchecked())}
        };

        loop {
            if let Ok(((op, l, r), i)) = token(binary_op).parse(input.clone()) {
                if l < min { break; }

                let (r_expr, i) = expr(r).parse(i)?;

                lhs = Expr::BinaryOp { 
                    left: Box::new(lhs), 
                    op, 
                    right: Box::new(r_expr)
                };
                input = i;
                continue;
            }

            if let Ok((l, i)) = token("ISNULL").map(|_| 1u8).parse(input.clone()) {
                if l < min { break; }
                lhs = Expr::IsNull(Box::new(lhs));
                input = i;
                continue;
            }

            if let Ok((is_not, i)) = opt_not.parse(input.clone()) {
                if let Ok((l, i)) = token("BETWEEN").map(|_| 1u8).parse(i.clone()) {
                    if l< min { break; }

                    let (l_expr, i) = expr(0).parse(i)?;
                    let (_, i) = token("AND").parse(i)?;
                    let (r_expr, i) = expr(0).parse(i)?;

                    lhs = Expr::Between { 
                        is_not, 
                        expr: Box::new(lhs), 
                        left: Box::new(l_expr), 
                        right: Box::new(r_expr)
                    };
                    input = i;
                    continue;
                }
            }

            break;
        }

        Ok((lhs, input))
    }
}


#[test]
fn test() {
    let r =expr(0).parse(" ( 1e2+2.2e2, 3* 5 )");
    println!("{:#?}", r);


}