use rtor::{
    Input, 
    Parser, 
    primitive::pure,
    combine::{opt, between, sepby1}, 
};

use crate::parser::tokenizer::tokenize;

use super::{expr::{Expr, expr}, Ident, ParseResult, tokenizer::TokenWithLocation, Keyword, ident, Punct, ParseError};


#[derive(Debug, Clone)]
pub struct Select {
    pub compound: Compound,
    pub order_by: Vec<OrderItem>,
    pub limit: Option<Limit>
}


#[derive(Debug, Clone)]
pub struct Limit {
    pub start: Expr,
    pub offset: Option<Expr>
}


#[derive(Debug, Clone)]
pub struct OrderItem {
    pub expr: Expr,
    pub asc: Option<bool>,
    pub nulls_first: Option<bool>
}


#[derive(Debug, Clone)]
pub enum Compound {
    Simple {
        distinct: bool,
        select: Vec<SelectItem>,
        from: Option<FromItem>,
        r#where: Option<Expr>,
        group_by: Vec<Expr>,
        having: Option<Expr>
    },
    Set {
        op: SetOperator,
        left: Box<Compound>,
        right: Box<Compound>
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
    LeftOuter { natural: bool },
    RightOuter { natural: bool },
    FullOuter { natural: bool },
    Inner { natural: bool },
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
pub enum SetOperator {
    Union,
    UnionAll,
    Intersect,
    Except
}


pub fn stmt_select<I>(input: I) -> ParseResult<Select, I> 
where I: Input<Token = TokenWithLocation>
{
    let (compound, i) = compound(0).parse(input)?;
    let (order_by, i) = opt(order_by).parse(i)?;
    let (limit, i) = opt(limit).parse(i)?;

    Ok((Select {compound, order_by: order_by.unwrap_or(vec![]), limit }, i))
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


        let (select, i) = sepby1(select_item, Punct::Comma).parse(i)?;

        let (from, i) = opt(Keyword::From.andr(from_item(0))).parse(i)?;

        let (r#where, i) = opt(Keyword::Where.andr(expr(0))).parse(i)?;

        let (group_by, i) = opt(Keyword::Group.andr(Keyword::By).andr(sepby1(expr(0), Punct::Comma))).parse(i)?;

        let (having, mut input) = opt(Keyword::Having.andr(expr(0))).parse(i)?;

        let mut left = Compound::Simple { 
            distinct, 
            select, 
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
    let tokens = tokenize("select * from (select 2), fuck").unwrap();
    println!("{:#?}", stmt_select.parse(tokens.as_slice()));
    // println!("{:#?}", tokens)
}