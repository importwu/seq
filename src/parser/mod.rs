use std::collections::HashMap;
use std::fmt;

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
        sep_by, ref_mut,
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

use lazy_static::lazy_static;

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
    condition: Expr,
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
        when: Vec<WhenCause>,
        r#else: Option<Box<Expr>>,
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
    order_by: Vec<OrderItem>,
    limit: Option<Limit>
}

#[derive(Debug)]
pub struct Limit {
    expr: Expr,
    offset: Option<Expr>
}

#[derive(Debug)]
pub struct OrderItem {
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
    let (body, i) = select_body(0).parse(input)?;
    let (order_by, i) = token(option(order_by)).parse(i)?;
    let (limit, i) = token(option(limit)).parse(i)?;

    Ok((Select {body, order_by: order_by.unwrap_or(vec![]), limit}, i))
}

fn order_by<I>(input: I) -> ParseResult<Vec<OrderItem>, I> 
where I: Input<Token = char>
{
    let order_item = expr(0)
        .and(
        token(option(Keyword::Asc.map(|_| true)
                    .or(Keyword::Desc.map(|_| false))))
        )
        .and(
            token(option(
                Keyword::Nulls.andr(token(Keyword::First.map(|_| true).or(Keyword::Last.map(|_| false)) ))
            ))
        )
        .map(|((expr, asc), nulls_first)| OrderItem {expr, asc, nulls_first});

     sep_by1(order_item, token(','))
        .parse(input)
}

fn limit<I>(input: I) -> ParseResult<Limit, I> 
where I: Input<Token = char>
{
 todo!()
}

fn select_body<I>(min: u8) -> impl Parser<I, Output = SelectBody, Error = Error<I::Token>> 
where I: Input<Token = char>
{
    move |input: I| {
        let (_, i) = Keyword::Select.parse(input)?;

        let (distinct, i) = token(
            Keyword::Distinct.map(|_| true)
                .or(Keyword::All.map(|_| false))
                .or(pure(false))
            )
            .parse(i)?;


        let (select, i) = sep_by1(token(select_item), token(',')).parse(i)?;

        let (from, i) = token(option(Keyword::From.andr(token(from_item(0))))).parse(i)?;

        let (r#where, i) = token(option(Keyword::Where.andr(expr(0)))).parse(i)?;

        let (group_by, i) = token(option(Keyword::Group
            .andr(token(Keyword::By))
            .andr(sep_by1(expr(0), token(',')))))
            .parse(i)?;

        let (having, mut input) = token(option(Keyword::Having.andr(expr(0)))).parse(i)?;

        let mut left = SelectBody::Simple { 
            distinct, 
            select, 
            from, 
            r#where, 
            group_by: group_by.unwrap_or(vec![]), 
            having 
        };

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

        return Ok((left, input))
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
            .and(option(option(token(Keyword::As)).andr(token(ident))))
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

#[derive(Debug, PartialEq, Eq)]
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
    End
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Select => write!(f, "{}", "SELECT"),
            Keyword::Natural => write!(f, "{}", "NATURAL"),
            Keyword::Left => write!(f, "{}", "LEFT"),
            Keyword::Right => write!(f, "{}", "RIGHT"),
            Keyword::Full => write!(f, "{}", "FULL"),
            Keyword::Inner => write!(f, "{}", "INNER"),
            Keyword::Cross => write!(f, "{}", "CROSS"),
            Keyword::Outer => write!(f, "{}", "Outer"),
            Keyword::Join => write!(f, "{}", "JOIN"),
            Keyword::On => write!(f, "{}", "ON"),
            Keyword::Using => write!(f, "{}", "USING"),
            Keyword::As => write!(f, "{}", "AS"),
            Keyword::Distinct => write!(f, "{}", "DISTINCT"),
            Keyword::All => write!(f, "{}", "ALL"),
            Keyword::From => write!(f, "{}", "FROM"),
            Keyword::Where => write!(f, "{}", "WHERE"),
            Keyword::Group => write!(f, "{}", "GROUP"),
            Keyword::By => write!(f, "{}", "BY"),
            Keyword::Having => write!(f, "{}", "HAVING"),
            Keyword::Order => write!(f, "{}", "ORDER"),
            Keyword::Limit => write!(f, "{}", "LIMIT"),
            Keyword::Asc => write!(f, "{}", "ASC"),
            Keyword::Desc => write!(f, "{}", "DESC"),
            Keyword::Nulls => write!(f, "{}", "NULLS"),
            Keyword::First => write!(f, "{}", "FIRST"),
            Keyword::Last => write!(f, "{}", "LAST"),
            Keyword::Case => write!(f, "{}", "CASE"),
            Keyword::When => write!(f, "{}", "WHEN"),
            Keyword::Then => write!(f, "{}", "THEN"),
            Keyword::Else => write!(f, "{}", "ELSE"),
            Keyword::End => write!(f, "{}", "END"),
        }
    }
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
        keywords
    };
}



impl<I> Parser<I> for Keyword 
where
    I: Input<Token = char>
{
    type Output = ();
    type Error = Error<I::Token>;

    fn parse(&mut self, input: I) -> Result<(Self::Output, I), Self::Error> {
        let (word, i) = word.parse(input)?;

        match KEYWORDS.get(word.to_uppercase().as_str()) {
            Some(k) if k == self => Ok(((), i)),
            None | Some(_) => Err(Error::Custom(format!("expected keyword '{}', but found '{}'", self, word))),
        }
    }
}

#[test]
fn test_keyword() {
    println!("{:?}", Keyword::Select.parse("fselect"))
}



fn word<I>(input: I) -> ParseResult<String, I> 
where I: Input<Token = char>
{
    let first = |input: I| satisfy(|c: &char| c.is_alphabetic()).or('_').parse(input);
    let subsequent = skip_many(first.or(digit));
    recognize(first.andr(subsequent))
        .map(|i: I| i.tokens().collect::<String>())
        .parse(input)
}

#[test]
fn test_from_item() {

    println!("{:#?}", select_body(0).parse("select * from a,b on c where 2 group by 3 having 4"))
}

fn ident<I>(input: I) -> ParseResult<Ident, I> 
where I: Input<Token = char>
{
    let (start_quote, i) = option('"'.or('[').or('`')).parse(input)?;

    let end_quote = match start_quote {
        None => {
            let (word, i) = word.parse(i)?;
            
            if KEYWORDS.contains_key(word.to_uppercase().as_str()) {
                return Err(Error::Custom("expect identify".into()))
            }

            let ident = Ident {
                value: word,
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
        .map(|i: I| Ident { value: i.tokens().collect(), quote: start_quote })
        .andl(token(end_quote))
        .parse(i)
}


fn join_constraint<I>(input: I) -> ParseResult<JoinConstraint, I> 
where I: Input<Token = char>
{
    Keyword::On.andr(expr(0)).map(JoinConstraint::On)
        .or(Keyword::Using.andr(
            between(
                token('('),
                sep_by1(token(ident), token(',')),
                token(')')
            )
        ).map(JoinConstraint::Using))
        .parse(input)
}

fn join_op<I>(input: I) -> ParseResult<JoinOperator, I> 
where I: Input<Token = char>
{
    ','.map(|_| JoinOperator::Cross)
        .or(Keyword::Cross.andr(token(Keyword::Join)).map(|_| JoinOperator::Cross))
        .or(|input: I| {
            let (natural, i) = option(Keyword::Natural).parse(input)?;
            
            match natural {
                None => Keyword::Left.map(|_| JoinOperator::Left)
                    .or(Keyword::Right.map(|_| JoinOperator::Right))
                    .or(Keyword::Full.map(|_| JoinOperator::Full))
                    .andl(token(option(Keyword::Outer)))
                    .or(Keyword::Inner.map(|_| JoinOperator::Inner))
                    .or(pure(JoinOperator::Inner))
                    .andl(token(Keyword::Join))
                    .parse(i),
                Some(_) => token(
                        Keyword::Left.map(|_| JoinOperator::NaturalLeft)
                            .or(Keyword::Right.map(|_| JoinOperator::NaturalRight))
                            .or(Keyword::Full.map(|_| JoinOperator::NaturalFull))
                            .andl(token(option(Keyword::Outer)))
                            .or(Keyword::Inner.map(|_| JoinOperator::NaturalInner))
                            .or(pure(JoinOperator::NaturalInner))
                            .andl(token(Keyword::Join))
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
        .and(option(option(token(Keyword::As)).andr(token(ident))))
        .map(|(expr, alias)| SelectItem::Expr { expr, alias })
        .or('*'.map(|_| SelectItem::Wildcard))
        .or(ident.andl(token('.')).andl(token('*')).map(SelectItem::TableWildcard))
        .parse(input)
}


fn numeric_literal<I>(input: I) -> ParseResult<Literal, I> 
where I: Input<Token = char>
{
    let fraction = '.'.andr(skip_many(digit));
    let fraction1 = '.'.andr(skip_many1(digit));
    let exponent = 'E'.or('e').andr(opt('+'.or('-'))).andr(skip_many1(digit));

    let numeric = skip_many1(digit)
        .andr(opt(fraction))
        .or(fraction1)
        .andr(opt(exponent));

    recognize(numeric)
        .map(|i: I| {
            let s = i.tokens().collect::<String>();
            unsafe {
                s.parse::<i64>().map(Literal::Integer)
                    .or(s.parse::<f64>().map(Literal::Float))
                    .unwrap_unchecked()
            }
        })
        .parse(input)
}

fn binary_op<I>(mut input: I) -> ParseResult<(BinaryOperator, u8, u8), I> 
where I: Input<Token = char>
{
    // string_no_case("OR").map(|_| (BinaryOperator::Or, 1, 2))
    //     .or( string_no_case("AND").map(|_| (BinaryOperator::And, 3, 4)))
    //     .or('<'.map(|_| (BinaryOperator::Lt, 9, 10)))
    //     .or('>'.map(|_| (BinaryOperator::Gt, 9, 10)))
    //     .or("<=".map(|_| (BinaryOperator::LtEq, 9, 10)))
        // .or(">=".map(|_| (BinaryOperator::GtEq, 9, 10)))
        // .or('&'.map(|_| (BinaryOperator::BitwiseAnd, 13, 14)))
        // .or('|'.map(|_| (BinaryOperator::BitwiseOr, 13, 14)))
        // .or("<<".map(|_| (BinaryOperator::ShiftLeft, 13, 14)))
        // .or(">>".map(|_| (BinaryOperator::ShiftRight, 13, 14)))
        // .or('+'.map(|_| (BinaryOperator::Plus, 15, 16)))
        // .or('-'.map(|_| (BinaryOperator::Minus, 15, 16)))
        // .or('*'.map(|_| (BinaryOperator::Multiply, 17, 18)))
        // .or('/'.map(|_| (BinaryOperator::Divide, 17, 18)))
        // .or('%'.map(|_| (BinaryOperator::Modulo, 17, 18)))
        // .or("||".map(|_| (BinaryOperator::StringConcat, 19, 20)))
        // .parse(input)


        let op = match input.next() {
            None => return Err(Error::Eoi),
            Some('<') => match input.peek() {
                Some('<') => {input.next(); (BinaryOperator::ShiftLeft, 13, 14)},
                Some('=') => {input.next(); (BinaryOperator::LtEq, 9, 10)},
                _ => (BinaryOperator::Lt, 9, 10)
            }
            Some('>') => match input.peek() {
                Some('>') =>  {input.next(); (BinaryOperator::ShiftRight, 13, 14)},
                Some('=') => {input.next(); (BinaryOperator::GtEq, 9, 10)},
                _ => (BinaryOperator::Gt, 9, 10)
            }
            Some('&') =>  (BinaryOperator::BitwiseAnd, 13, 14),
            Some('|') => match input.peek() {
                Some('|') => {input.next(); (BinaryOperator::StringConcat, 19, 20)},
                _ => (BinaryOperator::BitwiseOr, 13, 14)
            }
            Some('+') => (BinaryOperator::Plus, 15, 16),
            Some('-') => (BinaryOperator::Minus, 15, 16),
            Some('*') => (BinaryOperator::Multiply, 17, 18),
            Some('/') => (BinaryOperator::Divide, 17, 18),
            Some('%') => (BinaryOperator::Modulo, 17, 18),
            Some(c) => return Err(Error::Unexpected(c))
        };

        Ok((op, input))
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
        '(', 
        sep_by1(token(expr(0)), token(',')), 
        token(')')
    )
    .map(Expr::Tuple)
    .parse(input)
}

fn expr_unary<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = char>
{
    let ((op, l), i) = unary_op.parse(input)?;
    token(expr(l)).map(|e| Expr::UnaryOp { op, expr: Box::new(e) }).parse(i)
}

fn expr_literal<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = char>
{
    numeric_literal.map(Expr::Literal).parse(input)
}

fn expr_case<I>(input: I) -> ParseResult<Expr, I>
where I: Input<Token = char>
{
    let (operand, i) = Keyword::Case.andr(token(option(expr(0).map(Box::new)))).parse(input)?;
    
    let when = Keyword::When.andr(token(expr(0)));
    let then = Keyword::Then.andr(token(expr(0)));
    let (when_then, i) = many1(token(when).and(token(then)).map(|(condition, result)| WhenCause { condition, result }))
        .parse(i)?;

    let (r#else, i) = token(option(Keyword::Else.andr(token(expr(0)).map(Box::new))))
        .parse(i)?;
    
    let (_, i) = token(Keyword::End).parse(i)?;
    Ok((Expr::Case { operand, when: when_then, r#else}, i))
}

#[test]
fn test_case() {
    println!("{:#?}", expr(0).parse("1+2"))
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



#[test]
fn test() {
    let (expr, i) = expr(0).parse("1+2+3").unwrap();
    println!("{:#?}", expr);

    // let mut visitor = TestVisitor(vec![]);

    // expr.accept(&mut visitor);

}