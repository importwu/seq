use super::token::{
    Literal,
    Ident
};

use super::data_type::DataType;

use super::dml::Query;

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
    pub condition: Expr,
    pub result: Expr
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