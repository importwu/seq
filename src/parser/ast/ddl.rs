use super::Ident;
use super::query::Query;
use super::DataType;
use super::expr::Expr;

pub struct CreateTable {
    pub temporary: bool,
    pub exists: bool,
    pub schema_name: Option<Ident>,
    pub table_name: Ident,
    pub definition: Definition
}

pub enum Definition {
    As(Query),
    Column {
        definition: Vec<ColumnDef>,
        table_constraints: Vec<TableConstraint>
    }
}

pub struct ColumnDef {
    pub name: Ident,
    pub data_type: DataType,
    pub constraints: Vec<ColumnConstraint>,
}

pub struct ColumnConstraint {
    name: Option<Ident>,
    option: ColumnOption
}

pub enum ColumnOption {
    PrimaryKey {
        asc: Option<bool>,
        auto_increment: bool,
    },
    ForeignKey {
        table_name: Ident,
        column_names: Vec<Ident>,
        on_update: Option<ReferentialAction>,
        on_delete: Option<ReferentialAction>
    },
    NotNull,
    Unique,
    Check(Expr),
    Default(Expr)
}

pub enum ReferentialAction {
    SetNull,
    SetDefault,
    Cascade,
    Restrict,
    NoAction
}

pub struct TableConstraint {
    name: Option<Ident>,
    option: TableOption
}

pub enum TableOption {
    PrimaryKey
}