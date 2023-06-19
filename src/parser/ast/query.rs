use super::expr::Expr;
use super::Ident;

#[derive(Debug, Clone)]
pub struct Query {
    pub body: QueryCore,
    pub order_by: Vec<OrderItem>,
    pub limit: Option<Expr>,
    pub offset: Option<Expr>
}

#[derive(Debug, Clone)]
pub struct OrderItem {
    pub expr: Expr,
    pub asc: Option<bool>,
    pub nulls_first: Option<bool>
}

#[derive(Debug, Clone)]
pub enum QueryCore {
    Select {
        distinct: bool,
        result: Vec<SelectItem>,
        from: Option<FromItem>,
        r#where: Option<Expr>,
        group_by: Vec<Expr>,
        having: Option<Expr>
    },
    Values(Vec<Vec<Expr>>),
    Compound {
        op: SetOperator,
        left: Box<QueryCore>,
        right: Box<QueryCore>
    }
}

#[derive(Debug, Clone)]
pub struct Table {
    pub name: Ident,
    pub alias: Option<Ident>
}

#[derive(Debug, Clone)]
pub enum FromItem {
    Table(Table),
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