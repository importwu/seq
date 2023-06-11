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
    String(String),
    Null
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub value: String,
    pub quote: Option<char>
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

#[derive(Debug, Clone)]
pub struct Query {
    pub body: Select,
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
pub enum Select {
    Select {
        distinct: bool,
        result: Vec<ResultItem>,
        from: Option<FromItem>,
        r#where: Option<Expr>,
        group_by: Vec<Expr>,
        having: Option<Expr>
    },
    Values(Vec<Vec<Expr>>),
    Compound {
        op: SetOperator,
        left: Box<Select>,
        right: Box<Select>
    }
}


#[derive(Debug, Clone)]
pub enum FromItem {
    Table {
        name: Ident,
        alias: Option<Ident>
    },
    Subquery {
        query: Box<Query>,
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
pub enum ResultItem {
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

pub enum Stmt {
    Select(Query),
    Insert {
        table_name: Ident,
        columns: Vec<Ident>,
        source: Box<Query>
    }
}