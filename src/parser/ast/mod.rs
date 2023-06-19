use self::{query::Query, ddl::CreateTable};

pub mod ddl;
pub mod dml;
pub mod expr;
pub mod query;


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub value: String,
    pub quote: Option<char>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Number(String),
    Boolean(bool),
    String(String),
    Null
}

#[derive(Debug, Clone, Copy)]
pub enum DataType {
    Integer,
    Float,
    String,
    Boolean,
}

pub enum Stmt {
    Select(Query),
    CreateTable(CreateTable)
}