use rtor::{
    Parser,
    Input,
    Error,
    ParseResult,
    combine::{
        opt,
        skip_many1,
        skip_many,
        sep_by1,
        option, 
        between,
        recognize, many1
    },
    primitive::{
        digit,
        hex,
        token,
        error
    }
};



#[derive(Debug)]
pub enum LiteralValue {
    Number(String)
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Plus,
    Mul,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Plus,
    Minus
}

#[derive(Debug)]
pub struct WhenCause {
    expr: Expr,
    result: Expr
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
        when_cause: Vec<WhenCause>,
        else_cause: Option<Box<Expr>>,
    }
}

// textLiteral:
//     (UNDERSCORE_CHARSET? textStringLiteral | NCHAR_TEXT) textStringLiteral*

// UNDERSCORE_CHARSET: UNDERLINE_SYMBOL [a-z0-9]+ 

//UNDERLINE_SYMBOL : '_'

//
fn unsigned_numeric_literal<I>(input: I) -> ParseResult<LiteralValue, I> 
where I: Input<Token = char>
{
    let fraction = '.'.andr(skip_many(digit));
    let fraction1 = '.'.andr(skip_many1(digit));
    let exponent = 'E'.or('e').andr(opt('+'.or('-'))).andr(skip_many1(digit));

    let unsigned_numeric = skip_many1(digit)
        .andr(opt(fraction))
        .or(fraction1)
        .andr(opt(exponent));

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

fn expr_unary<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = char>
{
    token(unary_op)
        .and_then(|(op, l)| 
            expr(l).map(move |e| Expr::UnaryOp { op, expr: Box::new(e) })
        )
        .parse(input)
}

fn expr_literal<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = char>
{
    token(unsigned_numeric_literal)
        .map(|v| Expr::LiteralValue(v))
        .parse(input)
}

fn expr_case<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = char>
{
    let (operand, i) = token("CASE").andr(option(expr(0).map(Box::new))).parse(input)?;
    let (when_cause, i) = many1(token("WHEN").andr(expr(0))
        .and(token("THEN").andr(expr(0))).map(|(expr, result)| WhenCause {expr, result}))
        .parse(i)?;
    let (else_cause, i) = option(token("ELSE").andr(expr(0).map(Box::new))).andl(token("END")).parse(i)?;
    Ok((Expr::Case { operand, when_cause, else_cause}, i))
}


//pratt parser
fn expr<I>(min: u8) -> impl Parser<I, Output = Expr, Error = Error<I::Token>>
where I: Input<Token = char>
{
    move |input: I| {

        let (mut lhs, mut input) = expr_literal
            .or(expr_unary)
            .or(expr_tuple)
            .or(expr_case)
            .parse(input)?;

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
    
    let r = expr(0).parse("CASE 1 WHEN CASE 2 WHEN 4 THEN 5 END THEN 3 END");
    println!("{:#?}", r);
}