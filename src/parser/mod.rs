mod tokenizer;
mod expr;
// use std::collections::HashMap;
// use std::fmt;

use rtor::{
    combine::{
        opt,
        skip_many1,
        skip_many,
        sep_by1,
        option, 
        between,
        recognize, 
        many1,
        pair,
        sep_by, 
        ref_mut,
    }
};

// #[derive(Debug)]
// pub enum Literal {
//     Integer(i64),
//     Float(f64),
//     String(String),
//     Boolean(bool)
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    StringConcat,
    Gt,
    Lt,
    GtEq,
    LtEq,
    Eq,
    NotEq,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    ShiftRight,
    ShiftLeft,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub struct WhenCause {
    condition: Expr,
    result: Expr
}


#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    BinaryOp {
        op: BinaryOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOperator,
        expr: Box<Expr>
    },
    Between {
        not: bool,
        expr: Box<Expr>,
        left: Box<Expr>,
        right: Box<Expr>
    },
    Tuple(Vec<Expr>),
    IsNull {
        not: bool,
        expr: Box<Expr>
    },
    IsDistinctFrom {
        not: bool,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Case {
        operand: Option<Box<Expr>>,
        when: Vec<WhenCause>,
        r#else: Option<Box<Expr>>,
    },
    InList {
        not: bool,
        expr: Box<Expr>,
        list: Vec<Expr>
    },
    // InSubquery {
    //     not: bool,
    //     expr: Box<Expr>,
    //     subquery: Box<Select>
    // },
    // Exists {
    //     not: bool,
    //     subquery: Box<Select>
    // },
    // Subquery(Box<Select>),
    Column {
        table: Option<Ident>,
        column: Ident
    },
    Collate {
        expr: Box<Expr>,
        collation: Ident
    },
    Is {
        not: bool,
        left: Box<Expr>,
        right: Box<Expr>
    },
    Like {
        not: bool,
        expr: Box<Expr>,
        pattern: Box<Expr>,
        escape: Option<Box<Expr>>,
    },
    Cast {
        expr: Box<Expr>,
        data_type: DataType,
    },
    Function(Function)
}

#[derive(Debug, Clone)]
pub enum Function {
    Simple {
        name: Ident,
        arg: FunctionArg
    },
    Aggregate {
        name: Ident,
        arg: FunctionArg,
        distinct: bool,
        filter: Option<Box<Expr>>
    }
}

#[derive(Debug, Clone)]
pub enum FunctionArg {
    List(Vec<Expr>),
    Wildcard
}


// #[derive(Debug)]
// pub struct Select {
//     body: SelectBody,
//     order_by: Vec<OrderItem>,
//     limit: Option<Limit>
// }

// #[derive(Debug)]
// pub struct Limit {
//     expr: Expr,
//     offset: Option<Expr>
// }

// #[derive(Debug)]
// pub struct OrderItem {
//     expr: Expr,
//     asc: Option<bool>,
//     nulls_first: Option<bool>
// }

// #[derive(Debug)]
// pub enum SelectBody {
//     Simple {
//         distinct: bool,
//         select: Vec<SelectItem>,
//         from: Option<FromItem>,
//         r#where: Option<Expr>,
//         group_by: Vec<Expr>,
//         having: Option<Expr>
//     },
//     Compound {
//         op: CompoundOperator,
//         left: Box<SelectBody>,
//         right: Box<SelectBody>
//     }
// }


// #[derive(Debug)]
// pub enum FromItem {
//     Table {
//         name: Ident,
//         alias: Option<Ident>
//     },
//     Subquery {
//         query: Box<Select>,
//         alias: Option<Ident>
//     },
//     Join {
//         op: JoinOperator,
//         left: Box<FromItem>,
//         right: Box<FromItem>,
//         constraint: Option<JoinConstraint>
//     }
// }


// #[derive(Debug)]
// pub enum JoinConstraint {
//     On(Expr),
//     Using(Vec<Ident>)
// }

// #[derive(Debug, Clone, Copy)]
// pub enum JoinOperator {
//     Left,
//     Right,
//     Full,
//     Inner,
//     NaturalLeft,
//     NaturalRight,
//     NaturalFull,
//     NaturalInner,
//     Cross
// }

// #[derive(Debug)]
// pub enum SelectItem {
//     Expr {
//         expr: Expr,
//         alias: Option<Ident>
//     },
//     Wildcard,
//     TableWildcard(Ident)
// }


// #[derive(Debug)]
// pub enum CompoundOperator {
//     Union,
//     UnionAll,
//     Intersect,
//     Except
// }


// fn stmt_select<I>(input: I) -> ParseResult<Select, I> 
// where I: Input<Token = char>
// {
//     let (body, i) = select_body(0).parse(input)?;
//     let (order_by, i) = token(option(order_by)).parse(i)?;
//     let (limit, i) = token(option(limit)).parse(i)?;

//     Ok((Select {body, order_by: order_by.unwrap_or(vec![]), limit}, i))
// }

// fn order_by<I>(input: I) -> ParseResult<Vec<OrderItem>, I> 
// where I: Input<Token = char>
// {
//     let order_item = expr(0)
//         .and(
//         token(option(Keyword::Asc.map(|_| true)
//                     .or(Keyword::Desc.map(|_| false))))
//         )
//         .and(
//             token(option(
//                 Keyword::Nulls.andr(token(Keyword::First.map(|_| true).or(Keyword::Last.map(|_| false)) ))
//             ))
//         )
//         .map(|((expr, asc), nulls_first)| OrderItem {expr, asc, nulls_first});

//      sep_by1(order_item, token(','))
//         .parse(input)
// }

// fn limit<I>(input: I) -> ParseResult<Limit, I> 
// where I: Input<Token = char>
// {
//  todo!()
// }

// fn select_body<I>(min: u8) -> impl Parser<I, Output = SelectBody, Error = Error<I::Token>> 
// where I: Input<Token = char>
// {
//     move |input: I| {
//         let (_, i) = Keyword::Select.parse(input)?;

//         let (distinct, i) = token(
//             Keyword::Distinct.map(|_| true)
//                 .or(Keyword::All.map(|_| false))
//                 .or(pure(false))
//             )
//             .parse(i)?;


//         let (select, i) = sep_by1(token(select_item), token(',')).parse(i)?;

//         let (from, i) = token(option(Keyword::From.andr(token(from_item(0))))).parse(i)?;

//         let (r#where, i) = token(option(Keyword::Where.andr(expr(0)))).parse(i)?;

//         let (group_by, i) = token(option(Keyword::Group
//             .andr(token(Keyword::By))
//             .andr(sep_by1(expr(0), token(',')))))
//             .parse(i)?;

//         let (having, mut input) = token(option(Keyword::Having.andr(expr(0)))).parse(i)?;

//         let mut left = SelectBody::Simple { 
//             distinct, 
//             select, 
//             from, 
//             r#where, 
//             group_by: group_by.unwrap_or(vec![]), 
//             having 
//         };

//         loop {
//             if let Ok((op, i)) = token(compound_op).parse(input.clone()) {
//                 if 1 < min { break; }
//                 let (right, i) = token(select_body(2)).parse(i)?;
//                 left = SelectBody::Compound { 
//                     op, 
//                     left: Box::new(left), 
//                     right: Box::new(right)
//                 };
//                 input = i;
//                 continue;
//             }
//             break;
//         }

//         return Ok((left, input))
//     }

// }

// fn compound_op<I>(input: I) -> ParseResult<CompoundOperator, I> 
// where I: Input<Token = char>
// {
//     string_no_case("UNION").map(|_| CompoundOperator::Union)
//         .or(string_no_case("UNION").andr(string_no_case("ALL")).map(|_| CompoundOperator::UnionAll))
//         .or(string_no_case("INTERSECT").map(|_| CompoundOperator::Intersect))
//         .or(string_no_case("EXCEPT").map(|_| CompoundOperator::Except))
//         .parse(input)
// }


// fn from_item<I>(min: u8) -> impl Parser<I, Output = FromItem, Error = Error<I::Token>>
// where I: Input<Token = char>
// {
//     move |input: I| {
//         let (mut left, mut input) = ident
//             .and(option(option(token(Keyword::As)).andr(token(ident))))
//             .map(|(name, alias)| FromItem::Table { name, alias })
//             .or(between(token('('), token(from_item(0)), token(')')))
//             .parse(input)?;

//         loop {
//             if let Ok((op, i)) =  token(join_op).parse(input.clone()) {
//                 if 1 < min { break; }

//                 let (right, i) = token(from_item(2)).parse(i)?;
//                 let (constraint, i) = token(option(join_constraint)).parse(i)?;

//                 left = FromItem::Join { 
//                     op, 
//                     left: Box::new(left), 
//                     right: Box::new(right), 
//                     constraint
//                 };
//                 input = i;
//                 continue;   
//             }
//             break;
//         }
//         return Ok((left, input))
//     }    
// }


// impl fmt::Display for Keyword {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Keyword::Select => write!(f, "{}", "SELECT"),
//             Keyword::Natural => write!(f, "{}", "NATURAL"),
//             Keyword::Left => write!(f, "{}", "LEFT"),
//             Keyword::Right => write!(f, "{}", "RIGHT"),
//             Keyword::Full => write!(f, "{}", "FULL"),
//             Keyword::Inner => write!(f, "{}", "INNER"),
//             Keyword::Cross => write!(f, "{}", "CROSS"),
//             Keyword::Outer => write!(f, "{}", "Outer"),
//             Keyword::Join => write!(f, "{}", "JOIN"),
//             Keyword::On => write!(f, "{}", "ON"),
//             Keyword::Using => write!(f, "{}", "USING"),
//             Keyword::As => write!(f, "{}", "AS"),
//             Keyword::Distinct => write!(f, "{}", "DISTINCT"),
//             Keyword::All => write!(f, "{}", "ALL"),
//             Keyword::From => write!(f, "{}", "FROM"),
//             Keyword::Where => write!(f, "{}", "WHERE"),
//             Keyword::Group => write!(f, "{}", "GROUP"),
//             Keyword::By => write!(f, "{}", "BY"),
//             Keyword::Having => write!(f, "{}", "HAVING"),
//             Keyword::Order => write!(f, "{}", "ORDER"),
//             Keyword::Limit => write!(f, "{}", "LIMIT"),
//             Keyword::Asc => write!(f, "{}", "ASC"),
//             Keyword::Desc => write!(f, "{}", "DESC"),
//             Keyword::Nulls => write!(f, "{}", "NULLS"),
//             Keyword::First => write!(f, "{}", "FIRST"),
//             Keyword::Last => write!(f, "{}", "LAST"),
//             Keyword::Case => write!(f, "{}", "CASE"),
//             Keyword::When => write!(f, "{}", "WHEN"),
//             Keyword::Then => write!(f, "{}", "THEN"),
//             Keyword::Else => write!(f, "{}", "ELSE"),
//             Keyword::End => write!(f, "{}", "END"),
//         }
//     }
// }


// fn join_constraint<I>(input: I) -> ParseResult<JoinConstraint, I> 
// where I: Input<Token = char>
// {
//     Keyword::On.andr(expr(0)).map(JoinConstraint::On)
//         .or(Keyword::Using.andr(
//             between(
//                 token('('),
//                 sep_by1(token(ident), token(',')),
//                 token(')')
//             )
//         ).map(JoinConstraint::Using))
//         .parse(input)
// }

// fn join_op<I>(input: I) -> ParseResult<JoinOperator, I> 
// where I: Input<Token = char>
// {
//     ','.map(|_| JoinOperator::Cross)
//         .or(Keyword::Cross.andr(token(Keyword::Join)).map(|_| JoinOperator::Cross))
//         .or(|input: I| {
//             let (natural, i) = option(Keyword::Natural).parse(input)?;
            
//             match natural {
//                 None => Keyword::Left.map(|_| JoinOperator::Left)
//                     .or(Keyword::Right.map(|_| JoinOperator::Right))
//                     .or(Keyword::Full.map(|_| JoinOperator::Full))
//                     .andl(token(option(Keyword::Outer)))
//                     .or(Keyword::Inner.map(|_| JoinOperator::Inner))
//                     .or(pure(JoinOperator::Inner))
//                     .andl(token(Keyword::Join))
//                     .parse(i),
//                 Some(_) => token(
//                         Keyword::Left.map(|_| JoinOperator::NaturalLeft)
//                             .or(Keyword::Right.map(|_| JoinOperator::NaturalRight))
//                             .or(Keyword::Full.map(|_| JoinOperator::NaturalFull))
//                             .andl(token(option(Keyword::Outer)))
//                             .or(Keyword::Inner.map(|_| JoinOperator::NaturalInner))
//                             .or(pure(JoinOperator::NaturalInner))
//                             .andl(token(Keyword::Join))
//                         )
//                     .parse(i),
//             }

//         })
//         .parse(input)
// }

// fn select_item<I>(input: I) -> ParseResult<SelectItem, I> 
// where I: Input<Token = char>
// {
//     expr(0)
//         .and(option(option(token(Keyword::As)).andr(token(ident))))
//         .map(|(expr, alias)| SelectItem::Expr { expr, alias })
//         .or('*'.map(|_| SelectItem::Wildcard))
//         .or(ident.andl(token('.')).andl(token('*')).map(SelectItem::TableWildcard))
//         .parse(input)
// }


// fn numeric_literal<I>(input: I) -> ParseResult<Literal, I> 
// where I: Input<Token = char>
// {
//     let fraction = '.'.andr(skip_many(digit));
//     let fraction1 = '.'.andr(skip_many1(digit));
//     let exponent = 'E'.or('e').andr(opt('+'.or('-'))).andr(skip_many1(digit));

//     let numeric = skip_many1(digit)
//         .andr(opt(fraction))
//         .or(fraction1)
//         .andr(opt(exponent));

//     recognize(numeric)
//         .map(|i: I| {
//             let s = i.tokens().collect::<String>();
//             unsafe {
//                 s.parse::<i64>().map(Literal::Integer)
//                     .or(s.parse::<f64>().map(Literal::Float))
//                     .unwrap_unchecked()
//             }
//         })
//         .parse(input)
// }

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
        Some(TokenWithLocation {token, location}) => return Err(ParseError("invalid binary op".into())),
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
        Some(TokenWithLocation {token, location}) => return Err(ParseError("invalid unary op".into())),
        None => return Err(ParseError("end of input".into()))
    };

    Ok((op, input))
}

fn opt_not<I>(input: I) -> ParseResult<bool, I> 
where I: Input<Token = TokenWithLocation>
{
    option(Keyword::Not)
        .map(|x| x.map_or(false, |_| true))
        .parse(input) 
}

fn expr_tuple<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = TokenWithLocation>
{
    between(
        Punct::LParen, 
        sep_by1(expr(0), Punct::Comma), 
        Punct::RParen
    )
    .map(Expr::Tuple)
    .parse(input)
}

fn expr_unary<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = TokenWithLocation>
{
    let ((op, l), i) = unary_op.parse(input)?;
    expr(l).map(|e| Expr::UnaryOp { op, expr: Box::new(e) }).parse(i)
}

fn expr_literal<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = TokenWithLocation>
{
    literal.map(Expr::Literal).parse(input)
}

fn expr_case<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = TokenWithLocation>
{
    let (operand, i) = Keyword::Case.andr(option(expr(0).map(Box::new))).parse(input)?;
    
    let when = Keyword::When.andr(expr(0));
    let then = Keyword::Then.andr(expr(0));
    let (when_then, i) = many1(when.and(then).map(|(condition, result)| WhenCause { condition, result })).parse(i)?;

    let (r#else, i) = option(Keyword::Else.andr(expr(0).map(Box::new))).parse(i)?;
    
    let (_, i) = Keyword::End.parse(i)?;
    Ok((Expr::Case { operand, when: when_then, r#else}, i))
}

fn expr_column<I>(input: I) -> ParseResult<Expr, I> 
where I: Input<Token = TokenWithLocation>
{
    option(ident.andl(Punct::Period))
        .and(ident)
        .map(|(table, column)| Expr::Column { table, column})
        .parse(input)
}

fn data_type<I>(mut input: I) -> ParseResult<DataType, I> 
where I: Input<Token = TokenWithLocation>
{
    match input.next() {
        Some(TokenWithLocation {token: Token::Ident(Ident {value, quote: None}), location: _}) => {
            match value.to_uppercase().as_str() {
                "INT" | "INTEGER" => Ok((DataType::Integer, input)),
                "FLOAT" => Ok((DataType::Float, input)),
                "BOOL" | "BOOLEAN" => Ok((DataType::Boolean, input)),
                "STRING" => Ok((DataType::String, input)),
                _ => return Err(ParseError("invalid datatype".into())),
            }   
        } 
        Some(TokenWithLocation {token, location}) => return Err(ParseError("invalid datatype".into())), 
        None => return Err(ParseError("end of input".into()))
    }
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

    let (distinct, i) = option(Keyword::Distinct).parse(i)?;
    
    let filter = |input: I| {
        Keyword::Filter
            .andr(between(
                Punct::LParen,
                Keyword::Where.andr(expr(0)),
                Punct::RParen
            ))
            .parse(input)
    };

    match distinct {
        Some(_) => {
            let (arg, i) = sep_by1(expr(0), Punct::Comma)
                .map(FunctionArg::List)
                .parse(i)?;
            let (_, i) = Punct::RParen.parse(i)?;

            let (filter, i) = option(filter).map(|o| o.map(Box::new)).parse(i)?;

            let function_expr = Expr::Function(
                Function::Aggregate { name, arg, distinct: true, filter }
            );
            Ok((function_expr, i))
        }
        None => {
            let (arg, i) = Punct::Star.map(|_| FunctionArg::Wildcard)
                .or(sep_by(expr(0), Punct::Comma).map(FunctionArg::List))
                .parse(i)?;
            let (_, i) = Punct::RParen.parse(i)?;
            let (filter, i) = option(filter).map(|o| o.map(Box::new)).parse(i)?;
            let function_expr = match filter {
                Some(_) => Function::Aggregate { name, arg, distinct: false, filter },
                None => Function::Simple { name, arg }
            };
            Ok((Expr::Function(function_expr), i))
        }
    }
}

// fn expr_exists<I>(input: I) -> ParseResult<Expr, I> 
// where I: Input<Token = char>
// {
//      let (not, i) = opt_not.parse(input)?;
//      token(string_no_case("EXISTS"))
//         .andr(between(
//             token('('),
//             stmt_select,
//             token(')')
//         ))
//         .map(|query| Expr::Exists { not, subquery: Box::new(query) })
//         .parse(i)
// }

static mut between_and: bool = false;

//pratt parser
fn expr<I>(min: u8) -> impl Parser<I, Output = Expr, Error = ParseError>
where I: Input<Token = TokenWithLocation>
{
    move |input: I| {

        let (mut lhs, mut input) = expr_literal
            .or(expr_unary)
            .or(expr_tuple)
            .or(expr_case)
            .or(expr_cast)
            // .or(expr_exists)
            .or(expr_function)
            .or(expr_column)
            .parse(input)?;

        loop {
            if let Ok(((op, l, r), i)) = binary_op.parse(input.clone()) {
                if op == BinaryOperator::And && unsafe { between_and } {
                    break;
                }

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

                let (not, i) = opt_not.parse(i)?;

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


            let ((not, i)) =  opt_not.parse(input.clone())?;

            if let Ok((_, i)) = Keyword::Null.parse(i.clone()) {
                if 7 < min { break; }
                lhs = Expr::IsNull {
                    not,
                    expr: Box::new(lhs)
                };
                input = i;
                continue;
            }

            if let Ok((_, i)) = Keyword::Like.parse(i.clone()) {
                if 8 < min { break; }

                let (mut pattern, i) = expr(7).parse(i)?;

                let (escape, i) = option(Keyword::Escape.andr(expr(0)))
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

            if let Ok((_, i)) = Keyword::Between.parse(i.clone()) {
                if 8 < min { break; }

                unsafe {
                    between_and = true;
                }

                let (mut l_expr, i) = expr(7).parse(i)?;

                let (_, i) = Keyword::And.parse(i)?;

                let (r_expr, i) = expr(0).parse(i)?;

                unsafe {
                    between_and = false;
                }

                lhs = Expr::Between { 
                    not, 
                    expr: Box::new(lhs), 
                    left: Box::new(l_expr), 
                    right: Box::new(r_expr)
                };
                input = i;
                continue;
            }

            break;
        }

        Ok((lhs, input))
    }
}


use std::{collections::HashMap};
use lazy_static::lazy_static;
use rtor::{
    Parser,
    Input,
    Error,
};

use tokenizer::{
    Token,
    TokenWithLocation
};

#[derive(Debug)]
pub struct ParseError(String);

type ParseResult<T, I> = Result<(T, I), ParseError>;

#[derive(Debug, Clone, Copy)]
pub enum DataType {
    Integer,
    Float,
    String,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Number(String),
    Boolean(bool),
    String(String)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub value: String,
    pub quote: Option<char>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Select,
    Natural,
    Left,
    Right,
    Full,
    Inner,
    Cross,
    Outer,
    Join,
    On,
    Using,
    As,
    Distinct,
    All,
    From,
    Where,
    Group,
    By,
    Having,
    Order,
    Limit,
    Asc,
    Desc,
    Nulls,
    First,
    Last,
    Case,
    When,
    Then,
    Else,
    End,
    And,
    Or,
    Not,
    Cast,
    Is,
    Between,
    Like,
    Escape,
    IsNull,
    NotNull,
    Null,
    Collate,
    Filter
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Punct {
    Colon,               // :
    Comma,               // ,
    Tilde,               // ~
    Plus,                // +
    Minus,               // -
    StringConcat,        // ||
    Star,                // *
    Slash,               // /
    Percent,             // %
    Ampersand,           // &
    Vertical,            // |
    ShiftLeft,           // <<
    ShiftRight,          // >>
    Lt,                  // <
    Gt,                  // >
    LtEq,                // <=
    GtEq,                // >=
    Eq,                  // =
    DoubleEq,            // ==
    NotEq,               // <>
    NotEq2,              // !=
    Period,              // .
    LParen,              // (
    RParen,              // )
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Keyword> = {
        let mut keywords = HashMap::new();
        keywords.insert("SELECT", Keyword::Select);
        keywords.insert("NATURAL", Keyword::Natural);
        keywords.insert("LEFT", Keyword::Left);
        keywords.insert("RIGHT", Keyword::Right);
        keywords.insert("FULL", Keyword::Full);
        keywords.insert("INNER", Keyword::Inner);
        keywords.insert("CROSS", Keyword::Cross);
        keywords.insert("OUTER", Keyword::Outer);
        keywords.insert("JOIN", Keyword::Join);
        keywords.insert("ON", Keyword::On);
        keywords.insert("USING", Keyword::Using);
        keywords.insert("AS", Keyword::As);
        keywords.insert("DISTINCT", Keyword::Distinct);
        keywords.insert("ALL", Keyword::All);
        keywords.insert("FROM", Keyword::From);
        keywords.insert("WHERE", Keyword::Where);
        keywords.insert("GROUP", Keyword::Group);
        keywords.insert("BY", Keyword::By);
        keywords.insert("HAVING", Keyword::Having);
        keywords.insert("ORDER", Keyword::Order);
        keywords.insert("LIMIT", Keyword::Limit);
        keywords.insert("CASE", Keyword::Case);
        keywords.insert("WHEN", Keyword::When);
        keywords.insert("THEN", Keyword::Then);
        keywords.insert("ELSE", Keyword::Else);
        keywords.insert("END", Keyword::End);
        keywords.insert("AND", Keyword::And);
        keywords.insert("OR", Keyword::Or);
        keywords.insert("NOT", Keyword::Not);
        keywords.insert("CAST", Keyword::Not);
        keywords.insert("IS", Keyword::Is);
        keywords.insert("BETWEEN", Keyword::Not);
        keywords.insert("LIKE", Keyword::Not);
        keywords.insert("ESCAPE", Keyword::Not);
        keywords.insert("ISNULL", Keyword::Not);
        keywords.insert("NOTNULL", Keyword::Not);
        keywords.insert("NULL", Keyword::Not);
        keywords.insert("COLLATE", Keyword::Collate);
        keywords.insert("FILTER", Keyword::Filter);
        keywords
    };
}

impl<I> Parser<I> for Keyword 
where I: Input<Token = TokenWithLocation>
{
    type Output = ();
    type Error = ParseError;

    fn parse(&mut self, mut input: I) -> Result<(Self::Output, I), Self::Error> {
        match input.next() {
            Some(TokenWithLocation {token: Token::Keyword(keyword), location: _ }) if *self == keyword => Ok(((), input)),
            Some(x) => Err(ParseError("expected keyword".into())),
            None => Err(ParseError("end of input".into()))
        }
    }
}

impl<I> Parser<I> for Punct 
where I: Input<Token = TokenWithLocation>
{
    type Output = ();
    type Error = ParseError;

    fn parse(&mut self, mut input: I) -> Result<(Self::Output, I), Self::Error> {
        match input.next() {
            Some(TokenWithLocation {token: Token::Punct(punct), location: _ }) if *self == punct => Ok(((), input)),
            Some(x) => Err(ParseError("expected punct".into())),
            None => Err(ParseError("end of input".into()))
        }
    }
}

fn ident<I>(mut input: I) -> ParseResult<Ident, I>
where I: Input<Token = TokenWithLocation>
{
    match input.next() {
        Some(TokenWithLocation {token: Token::Ident(ident), location: _ }) => Ok((ident, input)),
        _ => Err(ParseError("end of input".into()))
    }
}

fn literal<I>(mut input: I) -> ParseResult<Literal, I>
where I: Input<Token = TokenWithLocation>
{
    match input.next() {
        Some(TokenWithLocation {token: Token::Literal(literal), location: _ }) => Ok((literal, input)),
        _ => Err(ParseError("end of input".into()))
    }
}

#[test]
fn test() {
    let tokenizer = tokenizer::Tokenizer::new("1+2");
    let tokens = tokenizer.tokenize().expect("invalid token");
    println!("{:?}", expr(0).parse(tokens.as_slice()))
}