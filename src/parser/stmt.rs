use rtor::{Input, combine::{opt, between, sepby1}, Parser, primitive::pure};

use crate::parser::tokenizer::tokenize;

use super::{expr::{Expr, expr}, Ident, ParseResult, tokenizer::TokenWithLocation, Keyword, ident, Punct, ParseError};


pub enum Stmt {
    Select(Select)
}


#[derive(Debug, Clone)]
pub struct Select {
    body: SelectBody,
    order_by: Vec<OrderItem>,
    limit: Option<Limit>
}


#[derive(Debug, Clone)]
pub struct Limit {
    expr: Expr,
    offset: Option<Expr>
}


#[derive(Debug, Clone)]
pub struct OrderItem {
    expr: Expr,
    asc: Option<bool>,
    nulls_first: Option<bool>
}


#[derive(Debug, Clone)]
pub enum SelectBody {
    Simple {
        distinct: bool,
        select: Vec<SelectItem>,
        from: Option<FromItem>,
        r#where: Option<Expr>,
        group_by: Vec<Expr>,
        having: Option<Expr>
    },
    Compound {
        op: CompoundOperator,
        left: Box<SelectBody>,
        right: Box<SelectBody>
    }
}


#[derive(Debug, Clone)]
pub enum FromItem {
    Table {
        name: Ident,
        alias: Option<Ident>
    },
    Subquery {
        query: Box<Select>,
        alias: Option<Ident>
    },
    Join {
        op: JoinOperator,
        left: Box<FromItem>,
        right: Box<FromItem>,
        constraint: Option<JoinConstraint>
    }
}



#[derive(Debug, Clone)]
pub enum JoinConstraint {
    On(Expr),
    Using(Vec<Ident>)
}

#[derive(Debug, Clone, Copy)]
pub enum JoinOperator {
    Left,
    Right,
    Full,
    Inner,
    NaturalLeft,
    NaturalRight,
    NaturalFull,
    NaturalInner,
    Cross
}


#[derive(Debug, Clone)]
pub enum SelectItem {
    Expr {
        expr: Expr,
        alias: Option<Ident>
    },
    Wildcard,
    TableWildcard(Ident)
}

#[derive(Debug, Clone, Copy)]
pub enum CompoundOperator {
    Union,
    UnionAll,
    Intersect,
    Except
}


pub fn stmt<I>(input: I) -> ParseResult<Stmt, I> 
where I: Input<Token = TokenWithLocation>
{
    todo!()
}


pub fn stmt_select<I>(input: I) -> ParseResult<Select, I> 
where I: Input<Token = TokenWithLocation>
{
    let (body, i) = select_body(0).parse(input)?;
    let (order_by, i) = opt(order_by).parse(i)?;
    let (limit, i) = opt(limit).parse(i)?;

    Ok((Select {body, order_by: order_by.unwrap_or(vec![]), limit }, i))
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
        .and(opt(Keyword::Offset.andr(expr(0)).or(Punct::Comma.andr(expr(0))) ))
        .map(|(expr, offset)| Limit { expr, offset })
        .parse(input)
}

fn select_body<I>(min: u8) -> impl Parser<I, Output = SelectBody, Error = ParseError> 
where I: Input<Token = TokenWithLocation>
{
    move |input: I| {
        let (_, i) = Keyword::Select.parse(input)?;

        let (distinct, i) = Keyword::Distinct.map(|_| true)
            .or(Keyword::All.map(|_| false))
            .or(pure(false))
            .parse(i)?;


        let (select, i) = sepby1(select_item, Punct::Comma).parse(i)?;

        let (from, i) = opt(Keyword::From.andr(from_item(0))).parse(i)?;

        let (r#where, i) = opt(Keyword::Where.andr(expr(0))).parse(i)?;

        let (group_by, i) = opt(
             Keyword::Group
            .andr(Keyword::By)
            .andr(sepby1(expr(0), Punct::Comma))
        )
        .parse(i)?;

        let (having, mut input) = opt(Keyword::Having.andr(expr(0))).parse(i)?;

        let mut left = SelectBody::Simple { 
            distinct, 
            select, 
            from, 
            r#where, 
            group_by: group_by.unwrap_or(vec![]), 
            having 
        };

        loop {
            if let Ok((op, i)) = compound_op.parse(input.clone()) {
                if 1 < min { break; }
                let (right, i) = select_body(2).parse(i)?;
                left = SelectBody::Compound { 
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

fn compound_op<I>(input: I) -> ParseResult<CompoundOperator, I> 
where I: Input<Token = TokenWithLocation>
{
    Keyword::Union.map(|_| CompoundOperator::Union)
        .or(Keyword::Union.andr(Keyword::All).map(|_| CompoundOperator::UnionAll))
        .or(Keyword::Intersect.map(|_| CompoundOperator::Intersect))
        .or(Keyword::Except.map(|_| CompoundOperator::Except))
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
        .or(Keyword::Using.andr(
            between(
                Punct::LParen,
                sepby1(ident, Punct::Comma),
                Punct::RParen
            )
        )
        .map(JoinConstraint::Using))
        .parse(input)
}

fn join_op<I>(input: I) -> ParseResult<JoinOperator, I> 
where I: Input<Token = TokenWithLocation>
{
    Punct::Comma.map(|_| JoinOperator::Cross)
        .or(Keyword::Cross.andr(Keyword::Join).map(|_| JoinOperator::Cross))
        .or(|input: I| {
            let (natural, i) = opt(Keyword::Natural).parse(input)?;
            match natural {
                None => Keyword::Left.map(|_| JoinOperator::Left)
                    .or(Keyword::Right.map(|_| JoinOperator::Right))
                    .or(Keyword::Full.map(|_| JoinOperator::Full))
                    .andl(opt(Keyword::Outer))
                    .or(Keyword::Inner.map(|_| JoinOperator::Inner))
                    .or(pure(JoinOperator::Inner))
                    .andl(Keyword::Join)
                    .parse(i),
                Some(_) => Keyword::Left.map(|_| JoinOperator::NaturalLeft)
                    .or(Keyword::Right.map(|_| JoinOperator::NaturalRight))
                    .or(Keyword::Full.map(|_| JoinOperator::NaturalFull))
                    .andl(opt(Keyword::Outer))
                    .or(Keyword::Inner.map(|_| JoinOperator::NaturalInner))
                    .or(pure(JoinOperator::NaturalInner))
                    .andl(Keyword::Join)
                    .parse(i),
            }
        })
        .parse(input)
}

fn select_item<I>(input: I) -> ParseResult<SelectItem, I> 
where I: Input<Token = TokenWithLocation>
{
    expr(0)
        .and(opt(opt(Keyword::As).andr(ident)))
        .map(|(expr, alias)| SelectItem::Expr { expr, alias })
        .or(Punct::Star.map(|_| SelectItem::Wildcard))
        .or(ident.andl(Punct::Period).andl(Punct::Star).map(SelectItem::TableWildcard))
        .parse(input)
}

#[test]
fn test() {
    let tokens = tokenize("select * from student where age > 20 group by name order by year limit 2 offset 3").unwrap();
    println!("{:#?}", stmt_select.parse(tokens.as_slice()));
    // println!("{:#?}", tokens)
}