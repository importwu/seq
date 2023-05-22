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
        error, string_no_case
    }
};


#[derive(Debug)]
pub enum Literal {
    Integer(i64),
    Float(f64)
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
        left: Box<Expr>,
        op: BinaryOperator,
        right: Box<Expr>
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
        subquery: Box<Query>
    },
    Exists {
        not: bool,
        subquery: Box<Query>
    },
    Subquery(Box<Query>),
    Is {
        not: bool,
        expr: Box<Expr>
    },
    Like {
        not: bool,
        expr: Box<Expr>,
        pattern: Box<Expr>,
        escape: Option<char>,
    },
    Cast {
        expr: Box<Expr>,
        data_type: DataType,
    },
    Function {}
}

#[derive(Debug)]
pub enum DataType {

}

#[derive(Debug)]
pub struct Query {

}

pub enum Stmt {
    Select(Box<Query>)
}

fn unsigned_numeric_literal<I>(input: I) -> ParseResult<Literal, I> 
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

    let numeric = unsafe {
        s.parse::<i64>().map(Literal::Integer)
        .or(s.parse::<f64>().map(Literal::Float))
        .unwrap_unchecked()
    };

    Ok((numeric, i))
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
        .or('+'.map(|_| (UnaryOperator::Plus, 21)))
        .or('-'.map(|_| (UnaryOperator::Minus, 22)))
        .or('~'.map(|_| (UnaryOperator::BitwiseNot, 23)))
        .parse(input)
}

fn opt_not<I>(input: I) -> ParseResult<bool, I> 
where I: Input<Token = char>
{
    option(token(string_no_case("NOT")))
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
            .parse(input)?;

        loop {
            if let Ok(((op, l, r), i)) = token(binary_op).parse(input.clone()) {
                if op == BinaryOperator::And && unsafe { between_and } {
                    break;
                }

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

            if let Ok((not, i)) = opt_not.parse(input.clone()) {

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

                    //todo escape

                    lhs = Expr::Like { 
                        not, 
                        expr: Box::new(lhs), 
                        pattern: Box::new(pattern), 
                        escape: None
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
    // let (expr, i) = expr(0).parse("1 BETWEEN 2 AND 3 BETWEEN 4 AND 5").unwrap();
    // let (expr, i) = expr(0).parse("1 BETWEEN 4 AND 5 BETWEEN 6 AND 7").unwrap();
    let (expr, i) = expr(0).parse(" 2 like 3 like 4").unwrap();
    println!("{:#?}", expr);

    // let mut visitor = TestVisitor(vec![]);

    // expr.accept(&mut visitor);

}