mod tokenizer;
mod expr;
mod ddl;
mod dml;
mod token;
mod data_type;

use rtor::{
    Parser,
    Input, combine::{sepby1, between, opt, many1, pair, sepby}, primitive::pure,
};

use token::{
    TokenWithLocation,
    Token,
    Keyword,
    Ident,
    Punct,
    Literal
};

use expr::{
    Expr,
    BinaryOperator,
    UnaryOperator,
    Function,
    FunctionArg,
    WhenCause
};

use dml::{
    OrderItem,
    SetOperator,
    Select,
    Limit,
    Query,
    FromItem,
    ResultItem,
    JoinConstraint,
    JoinOperator,
    Table
};

use tokenizer::tokenize;

use data_type::DataType;


#[derive(Debug)]
pub struct ParseError(String);

type ParseResult<T, I> = Result<(T, I), ParseError>;



impl<I> Parser<I> for Punct 
where I: Input<Token = TokenWithLocation>
{
    type Output = ();
    type Error = ParseError;
    
    fn parse(&mut self, mut input: I) -> Result<(Self::Output, I), Self::Error> {
        match input.next() {
            Some(TokenWithLocation {token: Token::Punct(punct), location: _ }) if *self == punct => Ok(((), input)),
            Some(TokenWithLocation { token, location }) => Err(ParseError(format!("{}, {}", location.line(), location.column()))),
            None => Err(ParseError("end of input".into()))
        }
    }
}

impl<I> Parser<I> for Keyword 
where I: Input<Token = TokenWithLocation>
{
    type Output = ();
    type Error = ParseError;
    
    fn parse(&mut self, mut input: I) -> Result<(Self::Output, I), Self::Error> {
        match input.next() {
            Some(TokenWithLocation {token: Token::Keyword(keyword), location: _ }) if *self == keyword => Ok(((), input)),
            Some(TokenWithLocation { token, location }) => Err(ParseError(format!("{}, {}", location.line(), location.column()))),
            None => Err(ParseError("end of input".into()))
        }
    }
}


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

pub fn select<I>(input: I) -> ParseResult<Query, I> 
where I: Input<Token = TokenWithLocation>
{
    let (body, i) = core(0).parse(input)?;
    let (order_by, i) = opt(order_by).parse(i)?;
    let (limit, i) = opt(limit).parse(i)?;

    Ok((Query {body, order_by: order_by.unwrap_or(vec![]), limit }, i))
}

fn order_by<I>(input: I) -> ParseResult<Vec<OrderItem>, I> 
where I: Input<Token = TokenWithLocation>
{
    let order_item = expr(0)
        .and(opt(Keyword::Asc.map(|_| true).or(Keyword::Desc.map(|_| false))))
        .and(opt(Keyword::Nulls.andr(Keyword::First.map(|_| true).or(Keyword::Last.map(|_| false)))))
        .map(|((expr, asc), nulls_first)| OrderItem {expr, asc, nulls_first});

    Keyword::Order.andr(Keyword::By)
        .andr(sepby1(order_item, Punct::Comma))
        .parse(input)
}

fn limit<I>(input: I) -> ParseResult<Limit, I> 
where I: Input<Token = TokenWithLocation>
{
    Keyword::Limit.andr(expr(0))
        .and(opt(Keyword::Offset.or(Punct::Comma).andr(expr(0))))
        .map(|(start, offset)| Limit { start, offset })
        .parse(input)
        
}

fn core<I>(min: u8) -> impl Parser<I, Output = Select, Error = ParseError> 
where I: Input<Token = TokenWithLocation>
{
    move |input: I| {
        let (_, i) = Keyword::Select.parse(input)?;

        let (distinct, i) = Keyword::Distinct.map(|_| true)
            .or(Keyword::All.map(|_| false))
            .or(pure(false))
            .parse(i)?;


        let (result, i) = sepby1(result_item, Punct::Comma).parse(i)?;

        let (from, i) = opt(Keyword::From.andr(from_item(0))).parse(i)?;

        let (r#where, i) = opt(Keyword::Where.andr(expr(0))).parse(i)?;

        let (group_by, i) = opt(Keyword::Group.andr(Keyword::By).andr(sepby1(expr(0), Punct::Comma))).parse(i)?;

        let (having, mut input) = opt(Keyword::Having.andr(expr(0))).parse(i)?;

        let mut left = Select::Select { 
            distinct, 
            result, 
            from, 
            r#where, 
            group_by: group_by.unwrap_or(vec![]), 
            having 
        };

        loop {
            if let Ok((op, i)) = set_op.parse(input.clone()) {
                if 1 < min { break; }
                let (right, i) = core(2).parse(i)?;
                left = Select::Compound { 
                    op, 
                    left: Box::new(left), 
                    right: Box::new(right)
                };
                input = i;
                continue;
            }
            break;
        }

        return Ok((left, input))
    }
}

fn set_op<I>(input: I) -> ParseResult<SetOperator, I> 
where I: Input<Token = TokenWithLocation>
{
    Keyword::Union.andr(Keyword::All).map(|_| SetOperator::UnionAll)
        .or(Keyword::Union.map(|_| SetOperator::Union))
        .or(Keyword::Intersect.map(|_| SetOperator::Intersect))
        .or(Keyword::Except.map(|_| SetOperator::Except))
        .parse(input)
}


fn from_item<I>(min: u8) -> impl Parser<I, Output = FromItem, Error = ParseError>
where I: Input<Token = TokenWithLocation>
{
    move |input: I| {
        let (mut left, mut input) = ident
            .and(opt(opt(Keyword::As).andr(ident)))
            .map(|(name, alias)| FromItem::Table(Table {name, alias}))
            .or(between(Punct::LParen, from_item(0), Punct::RParen))
            .or(
                between(Punct::LParen, select, Punct::RParen)
                    .and(opt(opt(Keyword::As).andr(ident)))
                    .map(|(query, alias)| FromItem::Subquery { query: Box::new(query), alias })
            )
            .parse(input)?;

        loop {
            if let Ok((op, i)) =  join_op.parse(input.clone()) {
                if 1 < min { break; }

                let (right, i) = from_item(2).parse(i)?;
                let (constraint, i) = opt(join_constraint).parse(i)?;

                left = FromItem::Join { 
                    op, 
                    left: Box::new(left), 
                    right: Box::new(right), 
                    constraint
                };
                input = i;
                continue;   
            }
            break;
        }
        return Ok((left, input))
    }    
}

fn join_constraint<I>(input: I) -> ParseResult<JoinConstraint, I> 
where I: Input<Token = TokenWithLocation>
{
    Keyword::On.andr(expr(0)).map(JoinConstraint::On)
        .or(Keyword::Using.andr(between(Punct::LParen,sepby1(ident, Punct::Comma),Punct::RParen)).map(JoinConstraint::Using))
        .parse(input)
}

fn join_op<I>(input: I) -> ParseResult<JoinOperator, I> 
where I: Input<Token = TokenWithLocation>
{
    Punct::Comma.map(|_| JoinOperator::Cross)
        .or(Keyword::Cross.andr(Keyword::Join).map(|_| JoinOperator::Cross))
        .or(|input: I| {
            let (natural, i) = opt(Keyword::Natural).map(|x| x.is_some()).parse(input)?;
            Keyword::Left.map(|_| JoinOperator::LeftOuter { natural })
                .or(Keyword::Right.map(|_| JoinOperator::RightOuter { natural }))
                .or(Keyword::Full.map(|_| JoinOperator::FullOuter { natural }))
                .andl(opt(Keyword::Outer))
                .or(Keyword::Inner.map(|_| JoinOperator::Inner { natural }))
                .or(pure(JoinOperator::Inner { natural }))
                .andl(Keyword::Join)
                .parse(i)
        })
        .parse(input)
}

fn result_item<I>(input: I) -> ParseResult<ResultItem, I> 
where I: Input<Token = TokenWithLocation>
{
    expr(0)
        .and(opt(opt(Keyword::As).andr(ident)))
        .map(|(expr, alias)| ResultItem::Expr { expr, alias })
        .or(Punct::Star.map(|_| ResultItem::Wildcard))
        .or(ident.andl(Punct::Period).andl(Punct::Star).map(ResultItem::TableWildcard))
        .parse(input)
}

#[test]
fn test() {
    let tokens = tokenize("2 in (select 2 and 3 as f)").unwrap();
    println!("{:#?}", expr(0).parse(tokens.as_slice()));
    // println!("{:#?}", tokens)
}


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