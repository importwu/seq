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
        recognize, 
        many1,
        pair,
        sep_by,
    },
    primitive::{
        digit,
        hex,
        token,
        error, 
        string_no_case, 
        alpha,
        anychar,
        satisfy,
        take_while,
        oneof,
        pure,
        space
    }
};


#[derive(Debug)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool)
}

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

#[derive(Debug)]
pub struct WhenCause {
    expr: Expr,
    result: Expr
}


#[derive(Debug)]
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
        when_cause: Vec<WhenCause>,
        else_cause: Option<Box<Expr>>,
    },
    InList {
        not: bool,
        expr: Box<Expr>,
        list: Vec<Expr>
    },
    InSubquery {
        not: bool,
        expr: Box<Expr>,
        subquery: Box<Select>
    },
    Exists {
        not: bool,
        subquery: Box<Select>
    },
    Subquery(Box<Select>),
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum FunctionArg {
    List(Vec<Expr>),
    Wildcard
}

#[derive(Debug)]
pub enum DataType {
    Integer,
    Float,
    String,
    Boolean,
}

#[derive(Debug)]
pub struct Select {
    body: SelectBody,
    order_by: Vec<OrderBy>,
    limit: Limit
}

#[derive(Debug)]
pub struct Limit {
    expr: Expr,
    offset: Option<Expr>
}

#[derive(Debug)]
pub struct OrderBy {
    expr: Expr,
    asc: Option<bool>,
    nulls_first: Option<bool>
}

#[derive(Debug)]
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


#[derive(Debug)]
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


#[derive(Debug)]
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

#[derive(Debug)]
pub enum SelectItem {
    Expr {
        expr: Expr,
        alias: Option<Ident>
    },
    Wildcard,
    TableWildcard(Ident)
}


#[derive(Debug)]
pub enum CompoundOperator {
    Union,
    UnionAll,
    Intersect,
    Except
}

#[derive(Debug)]
pub struct Ident {
    pub value: String,
    pub quote: Option<char>
}


fn stmt_select<I>(input: I) -> ParseResult<Select, I> 
where I: Input<Token = char>
{
    
    todo!()
}

fn select_body<I>(min: u8) -> impl Parser<I, Output = SelectBody, Error = Error<I::Token>> 
where I: Input<Token = char>
{
    move |input: I| {
        let (_, i) = string_no_case("SELECT").parse(input)?;

        let (distinct, i) = token(
            string_no_case("DISTINCT").map(|_| true)
                .or(string_no_case("ALL").map(|_| false))
                .or(pure(false))
        ).parse(i)?;


        let (select, i) = sep_by1(token(select_item), token(',')).parse(i)?;

        let (_, i) = skip_many(space).parse(i)?;

        let (from, i) = option(string_no_case("FROM").andr(from_item(0))).parse(i)?;

        let (r#where, i) = option(string_no_case("WHERE").andr(expr(0))).parse(i)?;


        //error
        let (group_by, i) = option(string_no_case("GROUP")
            .andr(string_no_case("By"))
            .andr(sep_by1(expr(0), token(','))))
            .parse(i)?;

        let (having, i) = option(string_no_case("HAVING").andr(expr(0))).parse(i)?;

        let mut left = SelectBody::Simple { 
            distinct, 
            select, 
            from, 
            r#where, 
            group_by, 
            having 
        };

        let mut input = i;

        loop {
            if let Ok((op, i)) = token(compound_op).parse(input.clone()) {
                if 1 < min { break; }
                let (right, i) = token(select_body(2)).parse(i)?;
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

        return Ok((left, i))
    }

}

fn compound_op<I>(input: I) -> ParseResult<CompoundOperator, I> 
where I: Input<Token = char>
{
    string_no_case("UNION").map(|_| CompoundOperator::Union)
        .or(string_no_case("UNION").andr(string_no_case("ALL")).map(|_| CompoundOperator::UnionAll))
        .or(string_no_case("INTERSECT").map(|_| CompoundOperator::Intersect))
        .or(string_no_case("EXCEPT").map(|_| CompoundOperator::Except))
        .parse(input)
}


fn from_item<I>(min: u8) -> impl Parser<I, Output = FromItem, Error = Error<I::Token>>
where I: Input<Token = char>
{
    move |input: I| {
        let (mut left, mut input) = ident
            .and(option(option(token(string_no_case("AS"))).andr(token(ident))))
            .map(|(name, alias)| FromItem::Table { name, alias })
            .or(between(token('('), token(from_item(0)), token(')')))
            .parse(input)?;

        loop {
            if let Ok((op, i)) =  token(join_op).parse(input.clone()) {
                if 1 < min { break; }

                let (right, i) = token(from_item(2)).parse(i)?;
                let (constraint, i) = token(option(join_constraint)).parse(i)?;

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

#[test]
fn test_from_item() {

    println!("{:#?}", from_item(0).parse("a,(b,c),d"))
}

fn join_constraint<I>(input: I) -> ParseResult<JoinConstraint, I> 
where I: Input<Token = char>
{
    string_no_case("ON").andr(expr(0)).map(JoinConstraint::On)
        .or(string_no_case("USING").andr(between(
            token('('),
            sep_by1(token(ident), token(',')),
        token(')')
        )).map(JoinConstraint::Using))
        .parse(input)
}

fn join_op<I>(input: I) -> ParseResult<JoinOperator, I> 
where I: Input<Token = char>
{
    ','.map(|_| JoinOperator::Cross)
        .or(string_no_case("CROSS").andr(token(string_no_case("JOIN"))).map(|_| JoinOperator::Cross))
        .or(|input: I| {
            let (natural, i) = option(string_no_case("NATURAL")).parse(input)?;
            
            match natural {
                None => string_no_case("LEFT").map(|_| JoinOperator::Left)
                    .or(string_no_case("RIGHT").map(|_| JoinOperator::Right))
                    .or(string_no_case("FULL").map(|_| JoinOperator::Full))
                    .andl(token(option(string_no_case("OUTER"))))
                    .or(string_no_case("INNER").map(|_| JoinOperator::Inner))
                    .or(pure(JoinOperator::Inner))
                    .andl(token(string_no_case("JOIN")))
                    .parse(i),
                Some(_) => token(string_no_case("LEFT").map(|_| JoinOperator::NaturalLeft)
                        .or(string_no_case("RIGHT").map(|_| JoinOperator::NaturalRight))
                        .or(string_no_case("FULL").map(|_| JoinOperator::NaturalFull))
                        .andl(token(option(string_no_case("OUTER"))))
                        .or(string_no_case("INNER").map(|_| JoinOperator::NaturalInner))
                        .or(pure(JoinOperator::NaturalInner))
                        .andl(token(string_no_case("JOIN")))
                    )
                    .parse(i),
            }

        })
        .parse(input)
}



fn select_item<I>(input: I) -> ParseResult<SelectItem, I> 
where I: Input<Token = char>
{
    expr(0)
        .and(option(option(token(string_no_case("AS"))).andr(token(ident))))
        .map(|(expr, alias)| SelectItem::Expr { expr, alias })
        .or('*'.map(|_| SelectItem::Wildcard))
        .or(ident.andl(token('.')).andl(token('*')).map(SelectItem::TableWildcard))
        .parse(input)
}

fn ident<I>(input: I) -> ParseResult<Ident, I> 
where I: Input<Token = char>
{
    let (start_quote, i) = option('"'.or('[').or('`')).parse(input)?;

    let end_quote = match start_quote {
        None => {
            let ident_part = oneof("_@#")
                .or(satisfy(|c: &char| c.is_alphabetic()))
                .andr(skip_many(oneof("_@#$").or(satisfy(|c: &char| c.is_alphabetic())).or(digit)));
            let (ident_part, i) = recognize(ident_part)
                .map(|o: I| o.tokens().collect::<String>())
                .parse(i)?;
            
            //check keyword todo!

            let ident = Ident {
                value: ident_part,
                quote: None
            };
            return Ok((ident, i))            
        }
        Some('"') => '"',
        Some('[') => ']',
        Some('`') => '`',
        _ => unreachable!()
    };

    token(take_while(move|c| *c != end_quote && *c != ' '))
        .map(|o: I| Ident { value: o.tokens().collect(), quote: start_quote })
        .andl(token(end_quote))
        .parse(i)
}

fn numeric_literal<I>(input: I) -> ParseResult<Literal, I> 
where I: Input<Token = char>
{
    let fraction = '.'.andr(skip_many(digit));
    let fraction1 = '.'.andr(skip_many1(digit));
    let exponent = 'E'.or('e').andr(opt('+'.or('-'))).andr(skip_many1(digit));

    let unsigned_numeric = skip_many1(digit)
        .andr(opt(fraction))
        .or(fraction1)
        .andr(opt(exponent));

    recognize(unsigned_numeric)
        .map(|o: I| {
            let s = o.tokens().collect::<String>();
            unsafe {
                s.parse::<i64>().map(Literal::Integer)
                    .or(s.parse::<f64>().map(Literal::Float))
                    .unwrap_unchecked()
            }
        })
        .parse(input)
}

fn binary_op<I>(input: I) -> ParseResult<(BinaryOperator, u8, u8), I> 
where I: Input<Token = char>
{
    string_no_case("OR").map(|_| (BinaryOperator::Or, 1, 2))
        .or( string_no_case("AND").map(|_| (BinaryOperator::And, 3, 4)))
        .or('<'.map(|_| (BinaryOperator::Lt, 9, 10)))
        .or('>'.map(|_| (BinaryOperator::Gt, 9, 10)))
        // .or("<=".map(|_| (BinaryOperator::LtEq, 9, 10)))
        // .or(">=".map(|_| (BinaryOperator::GtEq, 9, 10)))
        // .or('&'.map(|_| (BinaryOperator::BitwiseAnd, 13, 14)))
        // .or('|'.map(|_| (BinaryOperator::BitwiseOr, 13, 14)))
        // .or("<<".map(|_| (BinaryOperator::ShiftLeft, 13, 14)))
        // .or(">>".map(|_| (BinaryOperator::ShiftRight, 13, 14)))
        .or('+'.map(|_| (BinaryOperator::Plus, 15, 16)))
        .or('-'.map(|_| (BinaryOperator::Minus, 15, 16)))
        .or('*'.map(|_| (BinaryOperator::Multiply, 17, 18)))
        .or('/'.map(|_| (BinaryOperator::Divide, 17, 18)))
        // .or('%'.map(|_| (BinaryOperator::Modulo, 17, 18)))
        // .or("||".map(|_| (BinaryOperator::StringConcat, 19, 20)))
        .parse(input)
}

fn unary_op<I>(input: I) -> ParseResult<(UnaryOperator, u8), I> 
where I: Input<Token = char>
{
     string_no_case("NOT").map(|_| (UnaryOperator::Not, 5))
        .or('+'.map(|_| (UnaryOperator::Plus, 23)))
        .or('-'.map(|_| (UnaryOperator::Minus, 23)))
        .or('~'.map(|_| (UnaryOperator::BitwiseNot, 23)))
        .parse(input)
}

fn opt_not<I>(input: I) -> ParseResult<bool, I> 
where I: Input<Token = char>
{
    option(token(string_no_case("NOT")))
        .map(|x| x.map_or(false, |_| true))
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
    token(numeric_literal)
        .map(Expr::Literal)
        .parse(input)
}

fn expr_case<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = char>
{
    let (operand, i) = token(string_no_case("CASE")).andr(option(expr(0).map(Box::new))).parse(input)?;
    let (when_cause, i) = many1(token(string_no_case("WHEN")).andr(expr(0))
        .and(token(string_no_case("THEN")).andr(expr(0))).map(|(expr, result)| WhenCause {expr, result}))
        .parse(i)?;
    let (else_cause, i) = option(token(string_no_case("ELSE")).andr(expr(0).map(Box::new))).andl(token(string_no_case("END"))).parse(i)?;
    Ok((Expr::Case { operand, when_cause, else_cause}, i))
}

fn expr_column<I>(input: I) -> ParseResult<Expr, I> 
where I: Input<Token = char>
{
    option(token(ident).andl(token('.')))
        .and(token(ident))
        .map(|(table, column)| Expr::Column { table, column})
        .parse(input)
}

fn data_type<I>(input: I) -> ParseResult<DataType, I> 
where I: Input<Token = char>
{
    string_no_case("INT")
        .andr(opt(string_no_case("EGER")))
        .map(|_| DataType::Integer)
        .or(string_no_case("FLOAT").map(|_| DataType::Float))
        .or(string_no_case("BOOL").andr(opt(string_no_case("EAN"))).map(|_| DataType::Boolean))
        .or(string_no_case("STRING").map(|_| DataType::String))
        .parse(input)
}

fn expr_cast<I>(input: I) -> ParseResult<Expr, I> 
where I: Input<Token = char>
{
    token(string_no_case("CAST"))
        .andr(between(token('('), pair(expr(0), token(string_no_case("AS")), token(data_type)), token(')')))
        .map(|(expr, data_type)| Expr::Cast { expr: Box::new(expr), data_type })
        .parse(input)
}

fn expr_function<I>(input: I) -> ParseResult<Expr, I> 
where I: Input<Token = char>
{
    let (name, i) = token(ident).parse(input)?;

    let (_, i) = token('(').parse(i)?;

    let (distinct, i) = option(token(string_no_case("DISTINCT"))).parse(i)?;
    
    let filter = |input: I| {
        token(string_no_case("FILTER"))
            .andr(between(
                token('('),
                token(string_no_case("WHERE")).andr(expr(0)),
                token(')')
            ))
            .parse(input)
    };

    match distinct {
        Some(_) => {
            let (arg, i) = sep_by1(expr(0), token(','))
                .map(FunctionArg::List)
                .parse(i)?;
            let (_, i) = token(')').parse(i)?;

            let (filter, i) = option(filter).map(|o| o.map(Box::new)).parse(i)?;

            let function_expr = Expr::Function(
                Function::Aggregate { name, arg, distinct: true, filter }
            );
            Ok((function_expr, i))
        }
        None => {
            let (arg, i) = token('*').map(|_|FunctionArg::Wildcard)
                .or(sep_by(expr(0), token(',')).map(FunctionArg::List))
                .parse(i)?;
            let (_, i) = token(')').parse(i)?;
            let (filter, i) = option(filter).map(|o| o.map(Box::new)).parse(i)?;
            let function_expr = match filter {
                Some(_) => Function::Aggregate { name, arg, distinct: false, filter },
                None => Function::Simple { name, arg }
            };
            Ok((Expr::Function(function_expr), i))
        }
    }
}

fn expr_exists<I>(input: I) -> ParseResult<Expr, I> 
where I: Input<Token = char>
{
     let (not, i) = opt_not.parse(input)?;
     token(string_no_case("EXISTS"))
        .andr(between(
            token('('),
            stmt_select,
            token(')')
        ))
        .map(|query| Expr::Exists { not, subquery: Box::new(query) })
        .parse(i)
}

static mut between_and: bool = false;

//pratt parser
fn expr<I>(min: u8) -> impl Parser<I, Output = Expr, Error = Error<I::Token>>
where I: Input<Token = char>
{
    move |input: I| {

        let (mut lhs, mut input) = expr_literal
            .or(expr_unary)
            .or(expr_tuple)
            .or(expr_case)
            .or(expr_cast)
            .or(expr_exists)
            .or(expr_function)
            .or(expr_column)
            .parse(input)?;

        loop {
            if let Ok(((op, l, r), i)) = token(binary_op).parse(input.clone()) {
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

            if let Ok((_, i)) = token(string_no_case("IS")).parse(input.clone()) { 
                
                if 7 < min { break }

                let (not, i) = opt_not.parse(i)?;

                if let Ok((_, i)) = token(string_no_case("DISTINCT"))
                    .andr(token(string_no_case("FROM")))
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


            if let Ok((_, i)) = token(string_no_case("COLLATE")).parse(input.clone()) {
                if 21 < min { break }

                let (collation, i) = token(ident).parse(i)?;

                lhs = Expr::Collate { 
                    expr: Box::new(lhs), 
                    collation 
                };

                input = i;
                continue;
            }

            if let Ok((_, i)) = token(string_no_case("ISNULL")).parse(input.clone()) {
                if 7 < min { break; }
                lhs = Expr::IsNull {
                    not: false,
                    expr: Box::new(lhs)
                };
                input = i;
                continue;
            }

            if let Ok((_, i)) = token(string_no_case("NOTNULL")).parse(input.clone()) {
                if 7 < min { break; }
                lhs = Expr::IsNull {
                    not: true,
                    expr: Box::new(lhs)
                };
                input = i;
                continue;
            }


            let ((not, i)) =  opt_not.parse(input.clone())?;

            if let Ok((_, i)) = token(string_no_case("NULL")).parse(i.clone()) {
                if 7 < min { break; }
                lhs = Expr::IsNull {
                    not,
                    expr: Box::new(lhs)
                };
                input = i;
                continue;
            }

            if let Ok((_, i)) = token(string_no_case("LIKE")).parse(i.clone()) {
                if 8 < min { break; }

                let (mut pattern, i) = expr(7).parse(i)?;

                let (escape, i) = option(token(string_no_case("ESCAPE")).andr(expr(0)))
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

            if let Ok((_, i)) = token(string_no_case("BETWEEN")).parse(i.clone()) {
                if 8 < min { break; }

                unsafe {
                    between_and = true;
                }

                let (mut l_expr, i) = expr(7).parse(i)?;

                let (_, i) = token(string_no_case("AND")).parse(i)?;

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

// use std::ops::ControlFlow;

// pub trait Visitor {
//     fn pre_visit_expr(&mut self, expr: &Expr) -> ControlFlow<()>;
//     fn post_visit_expr(&mut self, expr: &Expr);
// }

// struct TestVisitor(Vec<i64>);

// pub trait AstNode {
//     fn accept<V: Visitor>(&self, visitor: &mut V) -> ControlFlow<()>;
// }

// impl AstNode for Expr {
//     fn accept<V: Visitor>(&self, visitor: &mut V) -> ControlFlow<()> {
//         visitor.pre_visit_expr(self)?;
//         match self {
//             Expr::Between { not, expr, left, right } => {
//                 expr.accept(visitor)?;
//                 left.accept(visitor)?;
//                 right.accept(visitor)?;
//             }
//             Expr::Literal(_) => (),
//             _ => panic!("fuck")
//         }
//         visitor.post_visit_expr(self);
//         ControlFlow::Continue(())
//     }
// }

// impl Visitor for TestVisitor {
//     fn pre_visit_expr(&mut self, expr: &Expr) -> ControlFlow<()> {
//         match expr {
//             a@Expr::Between { not, expr, left, right } => {
//                 if *not {
//                     return ControlFlow::Break(())
//                 }
//                 println!("pre:{:?}", a);
//             }
//             a@Expr::Literal(_) => {
//                 println!("pre:{:?}", a);
//             }
//             _ => panic!("fuck")
//         }
//         ControlFlow::Continue(())
//     }

//     fn post_visit_expr(&mut self, expr: &Expr) {
//         println!("post:{:?}", expr)
//     }
// }

#[test]
fn test() {
    let (expr, i) = expr(0).parse("1+2+3").unwrap();
    println!("{:#?}", expr);

    // let mut visitor = TestVisitor(vec![]);

    // expr.accept(&mut visitor);

}