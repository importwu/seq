use rtor::{
    combine::{
        opt,
        skip_many1,
        skip_many,
        sepby1,
        between,
        recognize, 
        many1,
        pair,
        sepby, 
    }, Input, Parser
};

use super::{
    ast::{
        WhenCause,
        BinaryOperator,
        UnaryOperator,
        Expr,
        Function,
        FunctionArg,
        Literal,
        Ident,
        DataType,
        Select
    },
    tokenizer::{
         TokenWithLocation, 
         Token,
         tokenize
    },
    keyword::Keyword,
    punct::Punct,
    ParseError,
    literal,
    ident,
    ParseResult,
    data_type,
    stmt::select::select
};

fn binary_op<I>(mut input: I) -> ParseResult<(BinaryOperator, u8, u8), I> 
where I: Input<Token = TokenWithLocation>
{
    let op = match input.next() {
        Some(TokenWithLocation {token: Token::Keyword(Keyword::Or), location: _}) =>  (BinaryOperator::Or, 1, 2),
        Some(TokenWithLocation {token: Token::Keyword(Keyword::And), location: _}) =>  (BinaryOperator::And, 3, 4),
        Some(TokenWithLocation {token: Token::Punct(Punct::Lt), location: _}) => (BinaryOperator::Lt, 9, 10),
        Some(TokenWithLocation {token: Token::Punct(Punct::Gt), location: _}) => (BinaryOperator::Gt, 9, 10),
        Some(TokenWithLocation {token: Token::Punct(Punct::LtEq), location: _}) => (BinaryOperator::LtEq, 9, 10),
        Some(TokenWithLocation {token: Token::Punct(Punct::GtEq), location: _}) => (BinaryOperator::GtEq, 9, 10),
        Some(TokenWithLocation {token: Token::Punct(Punct::Ampersand), location: _}) => (BinaryOperator::BitwiseAnd, 13, 14),
        Some(TokenWithLocation {token: Token::Punct(Punct::Vertical), location: _}) => (BinaryOperator::BitwiseOr, 13, 14),
        Some(TokenWithLocation {token: Token::Punct(Punct::ShiftLeft), location: _}) => (BinaryOperator::ShiftLeft, 13, 14),
        Some(TokenWithLocation {token: Token::Punct(Punct::ShiftRight), location: _}) => (BinaryOperator::ShiftRight, 13, 14),
        Some(TokenWithLocation {token: Token::Punct(Punct::Plus), location: _}) => (BinaryOperator::Plus, 15, 16),
        Some(TokenWithLocation {token: Token::Punct(Punct::Minus), location: _}) => (BinaryOperator::Minus, 15, 16),
        Some(TokenWithLocation {token: Token::Punct(Punct::Star), location: _}) => (BinaryOperator::Multiply, 17, 18),
        Some(TokenWithLocation {token: Token::Punct(Punct::Slash), location: _}) => (BinaryOperator::Divide, 17, 18),
        Some(TokenWithLocation {token: Token::Punct(Punct::Percent), location: _}) => (BinaryOperator::Modulo, 17, 18),
        Some(TokenWithLocation {token: Token::Punct(Punct::StringConcat), location: _}) => (BinaryOperator::StringConcat, 19, 20),
        Some(TokenWithLocation {token:_, location:_}) => return Err(ParseError("invalid binary op".into())),
        None => return Err(ParseError("end of input".into()))
    };

    Ok((op, input))
} 

fn unary_op<I>(mut input: I) -> ParseResult<(UnaryOperator, u8), I> 
where I: Input<Token = TokenWithLocation>
{   
    let op = match input.next() {
        Some(TokenWithLocation {token: Token::Keyword(Keyword::Not), location: _}) =>  (UnaryOperator::Not, 5),
        Some(TokenWithLocation {token: Token::Punct(Punct::Plus), location: _}) =>  (UnaryOperator::Plus, 23),
        Some(TokenWithLocation {token: Token::Punct(Punct::Minus), location: _}) =>  (UnaryOperator::Minus, 23),
        Some(TokenWithLocation {token: Token::Punct(Punct::Tilde), location: _}) =>  (UnaryOperator::BitwiseNot, 23),
        Some(TokenWithLocation {token:_, location:_}) => return Err(ParseError("invalid unary op".into())),
        None => return Err(ParseError("end of input".into()))
    };

    Ok((op, input))
}


fn expr_tuple<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = TokenWithLocation>
{
    between(Punct::LParen, sepby1(expr(0), Punct::Comma), Punct::RParen)
        .map(Expr::Tuple)
        .parse(input)
}

fn expr_unary<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = TokenWithLocation>
{
    let ((op, l), i) = unary_op.parse(input)?;
    expr(l).map(|e| Expr::UnaryOp { op, expr: Box::new(e) }).parse(i)
}

fn expr_case<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = TokenWithLocation>
{
    let (operand, i) = Keyword::Case.andr(opt(expr(0).map(Box::new))).parse(input)?;
    
    let (when, i) = many1(
        Keyword::When
            .andr(expr(0))
            .and(Keyword::Then.andr(expr(0)))
            .map(|(condition, result)| WhenCause { condition, result })
    )
    .parse(i)?;

    let (r#else, i) = opt(Keyword::Else.andr(expr(0).map(Box::new))).parse(i)?;
    
    let (_, i) = Keyword::End.parse(i)?;
    Ok((Expr::Case { operand, when, r#else}, i))
}

fn expr_column<I>(input: I) -> ParseResult<Expr, I> 
where I: Input<Token = TokenWithLocation>
{
    opt(ident.andl(Punct::Period))
        .and(ident)
        .map(|(table, column)| Expr::Column { table, column})
        .parse(input)
}


fn expr_cast<I>(input: I) -> ParseResult<Expr, I> 
where I: Input<Token = TokenWithLocation>
{
    Keyword::Cast
        .andr(between(Punct::LParen, pair(expr(0), Keyword::As, data_type), Punct::RParen))
        .map(|(expr, data_type)| Expr::Cast { expr: Box::new(expr), data_type })
        .parse(input)
}

fn expr_function<I>(input: I) -> ParseResult<Expr, I> 
where I: Input<Token = TokenWithLocation>
{
    let (name, i) = ident.parse(input)?;

    let (_, i) = Punct::LParen.parse(i)?;

    let (distinct, i) = opt(Keyword::Distinct).parse(i)?;
    
    let filter = Keyword::Filter
        .andr(between(Punct::LParen, Keyword::Where.andr(expr(0)), Punct::RParen));
            
    match distinct {
        Some(_) => {
            let (arg, i) = sepby1(expr(0), Punct::Comma)
                .map(FunctionArg::List)
                .parse(i)?;
            let (_, i) = Punct::RParen.parse(i)?;
            let (filter, i) = opt(filter).map(|o| o.map(Box::new)).parse(i)?;
            Ok((Expr::Function(Function::Aggregate { name, arg, distinct: true, filter }), i))
        }
        None => {
            let (arg, i) = Punct::Star.map(|_| FunctionArg::Wildcard)
                .or(sepby(expr(0), Punct::Comma).map(FunctionArg::List))
                .parse(i)?;
            let (_, i) = Punct::RParen.parse(i)?;
            let (filter, i) = opt(filter).map(|o| o.map(Box::new)).parse(i)?;
            match filter {
                Some(_) => Ok((Expr::Function(Function::Aggregate { name, arg, distinct: false, filter }), i)),
                None => Ok((Expr::Function(Function::Simple { name, arg }), i))
            }
        }
    }
}

fn expr_exists<I>(input: I) -> ParseResult<Expr, I> 
where I: Input<Token = TokenWithLocation>
{
    let (not, i) =  opt(Keyword::Not).map(|x| x.is_some()).parse(input)?;
    Keyword::Exists.andr(between(Punct::LParen,select,Punct::RParen))
        .map(|query| Expr::Exists { not, subquery: Box::new(query) })
        .parse(i)
}

static mut PARSE_BETWEEN_EXPR: bool = false;

//pratt parser
pub fn expr<I>(min: u8) -> impl Parser<I, Output = Expr, Error = ParseError>
where I: Input<Token = TokenWithLocation>
{
    move |input: I| {

        let (mut lhs, mut input) = literal.map(Expr::Literal)
            .or(expr_unary)
            .or(expr_tuple)
            .or(expr_case)
            .or(expr_cast)
            .or(expr_exists)
            .or(expr_function)
            .or(expr_column)
            .parse(input)?;

        loop {
            if let Ok(((op, l, r), i)) = binary_op.parse(input.clone()) {
                if op == BinaryOperator::And && unsafe { PARSE_BETWEEN_EXPR } { break }

                if l < min { break }

                let (r_expr, i) = expr(r).parse(i)?;

                lhs = Expr::BinaryOp { 
                    left: Box::new(lhs), 
                    op, 
                    right: Box::new(r_expr)
                };
                input = i;
                continue;
            }

            if let Ok((_, i)) = Keyword::Is.parse(input.clone()) { 
                
                if 7 < min { break }

                let (not, i) =  opt(Keyword::Not).map(|x| x.is_some()).parse(i)?;

                if let Ok((_, i)) = Keyword::Distinct
                    .andr(Keyword::From)
                    .parse(i.clone()) 
                {                       
                    let (r_expr, i) = expr(8).parse(i)?;

                    lhs = Expr::IsDistinctFrom { 
                        not, 
                        left: Box::new(lhs), 
                        right: Box::new(r_expr) 
                    };
                    input = i;
                    continue;
                }

                let (r_expr, i) = expr(8).parse(i)?;

                lhs = Expr::Is { 
                    not, 
                    left: Box::new(lhs), 
                    right: Box::new(r_expr) 
                }; 
                input = i;
                continue;
            }


            if let Ok((_, i)) = Keyword::Collate.parse(input.clone()) {
                if 21 < min { break }

                let (collation, i) = ident.parse(i)?;

                lhs = Expr::Collate { 
                    expr: Box::new(lhs), 
                    collation 
                };

                input = i;
                continue;
            }

            if let Ok((_, i)) = Keyword::IsNull.parse(input.clone()) {
                if 7 < min { break; }
                lhs = Expr::IsNull {
                    not: false,
                    expr: Box::new(lhs)
                };
                input = i;
                continue;
            }

            if let Ok((_, i)) = Keyword::NotNull.parse(input.clone()) {
                if 7 < min { break; }
                lhs = Expr::IsNull {
                    not: true,
                    expr: Box::new(lhs)
                };
                input = i;
                continue;
            }


            let (not, i) =  opt(Keyword::Not).map(|x| x.is_some()).parse(input)?;
            input = i;

            if let Ok((_, i)) = Keyword::Null.parse(input.clone()) {
                if 7 < min { break; }
                lhs = Expr::IsNull {
                    not,
                    expr: Box::new(lhs)
                };
                input = i;
                continue;
            }

            if let Ok((_, i)) = Keyword::Like.parse(input.clone()) {
                if 8 < min { break; }

                let (pattern, i) = expr(7).parse(i)?;

                let (escape, i) = opt(Keyword::Escape.andr(expr(0)))
                    .map(|o| o.map(Box::new))
                    .parse(i)?;

                lhs = Expr::Like { 
                    not, 
                    expr: Box::new(lhs), 
                    pattern: Box::new(pattern), 
                    escape
                };

                input = i;
                continue;
            }

            if let Ok((_, i)) = Keyword::Between.parse(input.clone()) {
                if 8 < min { break; }

                unsafe { PARSE_BETWEEN_EXPR = true }

                let (lexpr, i) = expr(7).parse(i)?;

                let (_, i) = Keyword::And.parse(i)?;

                let (rexpr, i) = expr(0).parse(i)?;

                unsafe { PARSE_BETWEEN_EXPR = false }

                lhs = Expr::Between { 
                    not, 
                    expr: Box::new(lhs), 
                    left: Box::new(lexpr), 
                    right: Box::new(rexpr)
                };
                input = i;
                continue;
            }

            if let Ok((_, i)) = Keyword::In.parse(input.clone()) {
                let (_, i) = Punct::LParen.parse(i)?;

                match select.parse(i.clone()) {
                    Ok((select, i)) => {
                        lhs = Expr::InSubquery { 
                            not, 
                            expr: Box::new(lhs), 
                            subquery: Box::new(select)
                        };
                        input = i;
                    }
                    Err(_) => {
                        let (list, i) = sepby(expr(0), Punct::Comma).parse(i)?;
                        lhs = Expr::InList { 
                            not, 
                            expr: Box::new(lhs), 
                            list
                        };
                        input = i;
                    }
                }
                
                let (_, i) = Punct::RParen.parse(input)?;
                input = i;
                continue;
            }

            break;
        }

        Ok((lhs, input))
    }
}


#[test]
fn test() {
    let tokens = tokenize("2 in (select 2 and 3 as f)").unwrap();//1 between 2 between 3 and 4 and 6 between 7 and 8
    println!("{:#?}", expr(0).parse(tokens.as_slice()));
    // println!("{:#?}", tokens)
}