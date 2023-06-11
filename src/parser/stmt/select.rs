use rtor::{
    Input, 
    Parser, 
    primitive::pure,
    combine::{opt, between, sepby1}, 
};

use crate::parser::tokenizer::tokenize;

use super::super::ast::{
    Expr,
    Select,
    Limit,
    OrderItem,
    Compound,
    FromItem,
    JoinConstraint,
    JoinOperator,
    ResultItem,
    SetOperator,
    Ident
};

use super::super::tokenizer::{
    TokenWithLocation,
    Token
};

use super::super::{
    keyword::Keyword,
    ident,
    punct::Punct,
    ParseError,
    ParseResult,
    expr::expr,
};


pub fn stmt_select<I>(input: I) -> ParseResult<Select, I> 
where I: Input<Token = TokenWithLocation>
{
    let (compound, i) = compound(0).parse(input)?;
    let (order_by, i) = opt(order_by).parse(i)?;
    let (limit, i) = opt(limit).parse(i)?;

    Ok((Select {body: compound, order_by: order_by.unwrap_or(vec![]), limit }, i))
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

fn compound<I>(min: u8) -> impl Parser<I, Output = Compound, Error = ParseError> 
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

        let mut left = Compound::Simple { 
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
                let (right, i) = compound(2).parse(i)?;
                left = Compound::Set { 
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
            .map(|(name, alias)| FromItem::Table { name, alias })
            .or(between(Punct::LParen, from_item(0), Punct::RParen))
            .or(
                between(Punct::LParen, stmt_select, Punct::RParen)
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
    let tokens = tokenize("select 1 intersect select 2").unwrap();
    println!("{:#?}", stmt_select.parse(tokens.as_slice()));
    // println!("{:#?}", tokens)
}