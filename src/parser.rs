use crate::tokeniser::{Keyword, Location, Token, TokenWithLocation, Tokeniser};

#[derive(PartialEq, Debug)]
pub enum Statement {
    Select(Select),
    Insert(Insert),
    Update(Update),
    Delete(Delete),
    Create(Create),
}

#[derive(PartialEq, Debug)]
enum Value {
    Number(String),
    String(String),
    Bool(bool),
    Null,
}

#[derive(PartialEq, Debug)]
enum Op {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(PartialEq, Debug)]
enum Ident {
    Single(String),
    Compound(Vec<String>),
}

#[derive(PartialEq, Debug)]
enum FunctionName {
    Min,
    Max,
    Sum,
    Avg,
    Count,
}

#[derive(PartialEq, Debug)]
struct Function {
    name: FunctionName,
    args: Vec<Expr>,
    distinct: bool,
}

#[derive(PartialEq, Debug)]
enum Expr {
    Wildcard,
    QualifiedWildcard(Ident),
    Ident(Ident),
    Value(Value),
    IsNull(Box<Expr>),
    IsNotNull(Box<Expr>),
    InList { expr: Box<Expr>, list: Vec<Expr>, negated: bool },
    Between { expr: Box<Expr>, negated: bool, low: Box<Expr>, high: Box<Expr> },
    BinaryOp { left: Box<Expr>, op: Op, right: Box<Expr> },
    SubQuery(Box<Query>),
    Function(Function),
}

#[derive(PartialEq, Debug)]
struct Query {
    projection: Vec<SelectItem>,
    from: FromTable,
    joins: Vec<Join>,
    filter: Option<Expr>,
    group: Vec<Expr>,
}

#[derive(PartialEq, Debug)]
enum FromTable {
    Table { name: Ident, alias: Option<String> },
    Derived { query: Box<Query>, alias: Option<String> },
}

#[derive(PartialEq, Debug)]
enum JoinConstraint {
    On(Expr),
    Using(Vec<Ident>),
}

#[derive(PartialEq, Debug)]
enum JoinType {
    Inner,
    // TODO: add more joins
}

#[derive(PartialEq, Debug)]
struct Join {
    from: FromTable,
    ty: JoinType,
    constraint: JoinConstraint,
}

#[derive(PartialEq, Debug)]
struct OrderByExpr {
    exprs: Vec<Expr>,
    desc: bool,
}

#[derive(PartialEq, Debug)]
enum SelectItem {
    Expr(Expr),
    AliasedExpr { expr: Expr, alias: String },
    QualifiedWildcard(Ident),
    Wildcard,
}

#[derive(PartialEq, Debug)]
pub struct Select {
    body: Query,
    order: Option<OrderByExpr>,
    limit: Option<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct Insert {
    table: Ident,
    rows: Vec<Vec<Expr>>,
}

#[derive(PartialEq, Debug)]
pub struct Update {
    table: Ident,
    set: Vec<Assignment>,
    filter: Option<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct Assignment {
    column: Ident,
    expr: Expr,
}

#[derive(PartialEq, Debug)]
pub struct Delete {
    table: Ident,
    filter: Option<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct Create {
    name: Ident,
    columns: Vec<ColumnDef>,
}

#[derive(PartialEq, Debug)]
enum ColumnType {
    Int,
    Varchar(u16),
}

#[derive(PartialEq, Debug)]
struct ColumnDef {
    ty: ColumnType,
    name: String,
    // TODO: constraints
}

#[derive(Debug)]
pub enum ParserError {
    TokeniserError(String),
    Unexpected(String),
}

struct Unexpected<'a>(&'a Token, &'a Location);

impl<'a> std::fmt::Display for Unexpected<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: unexpected token {:?}", self.1, self.0)
    }
}

impl<'a> From<Unexpected<'a>> for ParserError {
    fn from(value: Unexpected<'a>) -> Self {
        Self::Unexpected(value.to_string())
    }
}

pub type Result<T> = std::result::Result<T, ParserError>;

pub struct Parser {
    tokens: Vec<TokenWithLocation>,
    index: usize,
}

impl Parser {
    pub fn new(src: &str) -> Result<Self> {
        Tokeniser::new(src)
            .collect_with_location()
            .map_err(|e| ParserError::TokeniserError(e.to_string()))
            .map(|tokens| Self { tokens, index: 0 })
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>> {
        let mut statements = Vec::new();
        loop {
            let TokenWithLocation(token, location) = self.peek();
            statements.push(match token {
                Token::Keyword(kw) => match kw {
                    Keyword::Select => Statement::Select(self.parse_select()?),
                    Keyword::Insert => Statement::Insert(self.parse_insert()?),
                    Keyword::Update => Statement::Update(self.parse_update()?),
                    Keyword::Delete => Statement::Delete(self.parse_delete()?),
                    Keyword::Create => Statement::Create(self.parse_create()?),
                    _ => Err(Unexpected(&token, &location))?,
                },
                Token::Semicolon => continue,
                Token::Eof => break,
                _ => Err(Unexpected(&token, &location))?,
            });
        }

        Ok(statements)
    }

    fn parse_select(&mut self) -> Result<Select> {
        let body = self.parse_query()?;

        let order = if self.check_keywords(&[Keyword::Order, Keyword::By]) {
            let mut exprs = Vec::new();
            while {
                exprs.push(self.parse_expr(0)?);
                self.check_tokens(&[Token::Comma])
            } {}

            let mut order = OrderByExpr { exprs, desc: false };
            if self.check_keywords(&[Keyword::Desc]) {
                order.desc = true;
            } else if self.check_keywords(&[Keyword::Asc]) {
                order.desc = false;
            }

            Some(order)
        } else {
            None
        };

        let limit =
            if self.check_keywords(&[Keyword::Limit]) { Some(self.parse_expr(0)?) } else { None };

        Ok(Select { body, order, limit })
    }

    fn parse_query(&mut self) -> Result<Query> {
        self.parse_keywords(&[Keyword::Select])?;

        let projection = self.parse_projection()?;

        self.parse_keywords(&[Keyword::From])?;
        let from = self.parse_from()?;

        let joins = self.parse_joins()?;

        let filter =
            if self.check_keywords(&[Keyword::Where]) { Some(self.parse_expr(0)?) } else { None };

        let group = if self.check_keywords(&[Keyword::Group, Keyword::By]) {
            let mut exprs = Vec::new();
            while {
                exprs.push(self.parse_expr(0)?);
                self.check_tokens(&[Token::Comma])
            } {}

            exprs
        } else {
            vec![]
        };

        Ok(Query { projection, from, joins, filter, group })
    }

    fn parse_from(&mut self) -> Result<FromTable> {
        let TokenWithLocation(token, location) = self.next();
        let from = match token {
            Token::Ident(_) => {
                self.index -= 1;
                let name = self.parse_ident()?;

                let mut alias = None;
                if self.check_keywords(&[Keyword::As]) {
                    let TokenWithLocation(token, location) = self.next();
                    match token {
                        Token::Ident(a) => alias = Some(a),
                        _ => Err(Unexpected(&token, &location))?,
                    }
                }

                FromTable::Table { name, alias }
            }
            Token::LParen => {
                let query = self.parse_query().map(Box::new)?;
                self.parse_tokens(&[Token::RParen])?;

                let alias = if let TokenWithLocation(Token::Ident(alias), _) = self.peek() {
                    self.next();
                    Some(alias)
                } else {
                    None
                };

                FromTable::Derived { query, alias }
            }
            _ => Err(Unexpected(&token, &location))?,
        };

        Ok(from)
    }

    fn parse_joins(&mut self) -> Result<Vec<Join>> {
        let mut joins = Vec::new();

        if !self.check_keywords(&[Keyword::Join]) {
            return Ok(joins);
        }

        while {
            let from = self.parse_from()?;
            if self.check_keywords(&[Keyword::On]) {
                let constraint = JoinConstraint::On(self.parse_expr(0)?);
                let ty = JoinType::Inner;
                joins.push(Join { from, ty, constraint })
            } else if self.check_keywords(&[Keyword::Using]) {
                let mut columns = Vec::new();

                self.parse_tokens(&[Token::LParen])?;
                while {
                    columns.push(self.parse_ident()?);
                    self.check_tokens(&[Token::Comma])
                } {}
                self.parse_tokens(&[Token::RParen])?;

                let constraint = JoinConstraint::Using(columns);
                let ty = JoinType::Inner;
                joins.push(Join { from, ty, constraint })
            }

            self.check_keywords(&[Keyword::Join])
        } {}

        Ok(joins)
    }

    fn parse_insert(&mut self) -> Result<Insert> {
        self.parse_keywords(&[Keyword::Insert, Keyword::Into])?;

        let TokenWithLocation(token, location) = self.peek();
        let Token::Ident(_) = token else { Err(Unexpected(&token, &location))? };
        let table = self.parse_ident()?;

        self.parse_keywords(&[Keyword::Values])?;

        self.parse_tokens(&[Token::LParen])?;
        let mut rows = Vec::new();
        while {
            self.parse_tokens(&[Token::LParen])?;
            let mut exprs = Vec::new();
            while {
                exprs.push(self.parse_expr(0)?);
                self.check_tokens(&[Token::Comma])
            } {}
            rows.push(exprs);
            self.parse_tokens(&[Token::RParen])?;
            self.check_tokens(&[Token::Comma])
        } {}
        self.parse_tokens(&[Token::RParen])?;

        Ok(Insert { table, rows })
    }

    fn parse_update(&mut self) -> Result<Update> {
        self.parse_keywords(&[Keyword::Update])?;

        let table = self.parse_ident()?;

        self.parse_keywords(&[Keyword::Set])?;

        let mut set = Vec::new();
        while {
            let column = self.parse_ident()?;
            self.parse_tokens(&[Token::Eq])?;
            let expr = self.parse_expr(0)?;

            set.push(Assignment { column, expr });

            self.check_tokens(&[Token::Comma])
        } {}

        let filter =
            if self.check_keywords(&[Keyword::Where]) { Some(self.parse_expr(0)?) } else { None };

        Ok(Update { table, set, filter })
    }

    fn parse_delete(&mut self) -> Result<Delete> {
        self.parse_keywords(&[Keyword::Delete, Keyword::From])?;

        let table = self.parse_ident()?;

        let filter =
            if self.check_keywords(&[Keyword::Where]) { Some(self.parse_expr(0)?) } else { None };

        Ok(Delete { table, filter })
    }

    fn parse_create(&mut self) -> Result<Create> {
        self.parse_keywords(&[Keyword::Create, Keyword::Table])?;

        let name = self.parse_ident()?;

        self.parse_tokens(&[Token::LParen])?;
        let mut columns = Vec::new();
        while {
            columns.push(self.parse_column_def()?);
            self.check_tokens(&[Token::Comma])
        } {}
        self.parse_tokens(&[Token::RParen])?;

        Ok(Create { name, columns })
    }

    fn parse_column_def(&mut self) -> Result<ColumnDef> {
        let TokenWithLocation(token, location) = self.next();
        let name = match token {
            Token::Ident(name) => name,
            _ => Err(Unexpected(&token, &location))?,
        };

        let TokenWithLocation(token, location) = self.next();
        let ty = match token {
            Token::Keyword(Keyword::Int) => ColumnType::Int,
            Token::Keyword(Keyword::Varchar) => {
                self.parse_tokens(&[Token::LParen])?;
                let TokenWithLocation(token, location) = self.next();
                let max = match token {
                    Token::NumberLiteral(ref max) => {
                        max.parse().map_err(|_| Unexpected(&token, &location))?
                    }
                    _ => Err(Unexpected(&token, &location))?,
                };
                self.parse_tokens(&[Token::RParen])?;
                ColumnType::Varchar(max)
            }
            _ => Err(Unexpected(&token, &location))?,
        };

        Ok(ColumnDef { ty, name })
    }

    fn parse_projection(&mut self) -> Result<Vec<SelectItem>> {
        let mut items = Vec::new();
        while {
            items.push(self.parse_select_item()?);
            self.check_tokens(&[Token::Comma])
        } {}

        Ok(items)
    }

    fn parse_select_item(&mut self) -> Result<SelectItem> {
        // Try parse wildcard or qualified wildcard
        // Parse expr and optional alias
        let index = self.index;
        let TokenWithLocation(token, _) = self.next();
        match token {
            Token::Asterisk => return Ok(SelectItem::Wildcard),
            Token::Ident(a) => {
                // Try to parse a qualified ident, else reset index and parse_expr
                let mut parts = Vec::new();
                if self.check_tokens(&[Token::Dot]) {
                    let TokenWithLocation(b, location) = self.next();
                    match b {
                        Token::Ident(b) => parts = vec![a, b],
                        Token::Asterisk => {
                            return Ok(SelectItem::QualifiedWildcard(Ident::Single(a)))
                        }
                        _ => Err(Unexpected(&b, &location))?,
                    };

                    if self.check_tokens(&[Token::Dot]) {
                        let TokenWithLocation(c, location) = self.next();
                        match c {
                            Token::Ident(_) => {}
                            Token::Asterisk => {
                                return Ok(SelectItem::QualifiedWildcard(Ident::Compound(parts)))
                            }
                            _ => Err(Unexpected(&c, &location))?,
                        };
                    }
                }
            }
            _ => {}
        };

        self.index = index;
        let expr = self.parse_expr(0)?;
        if self.check_keywords(&[Keyword::As]) {
            let TokenWithLocation(token, location) = self.next();
            match token {
                Token::Ident(alias) => return Ok(SelectItem::AliasedExpr { expr, alias }),
                _ => Err(Unexpected(&token, &location))?,
            };
        };

        Ok(SelectItem::Expr(expr))
    }

    fn parse_expr(&mut self, prec: u8) -> Result<Expr> {
        let mut expr = self.parse_prefix()?;
        loop {
            let next_prec = self.next_prec()?;
            if prec >= next_prec {
                break;
            }
            expr = self.parse_infix(expr, next_prec)?;
        }

        Ok(expr)
    }

    fn parse_prefix(&mut self) -> Result<Expr> {
        let TokenWithLocation(token, location) = self.peek();
        let expr = match token {
            Token::Keyword(Keyword::False)
            | Token::Keyword(Keyword::True)
            | Token::Keyword(Keyword::Null)
            | Token::StringLiteral(_)
            | Token::NumberLiteral(_) => Expr::Value(self.parse_value()?),

            Token::Keyword(Keyword::Select) => Expr::SubQuery(self.parse_query().map(Box::new)?),

            Token::Keyword(kw) => match kw {
                Keyword::Min | Keyword::Max | Keyword::Sum | Keyword::Avg | Keyword::Count => {
                    Expr::Function(self.parse_function()?)
                }
                _ => Err(Unexpected(&token, &location))?,
            },

            Token::Ident(_) => {
                let ident = self.parse_ident()?;
                match &ident {
                    Ident::Compound(i)
                        if i.len() != 3 && self.check_tokens(&[Token::Dot, Token::Asterisk]) =>
                    {
                        Expr::QualifiedWildcard(ident)
                    }
                    _ => Expr::Ident(ident),
                }
            }

            Token::Asterisk => {
                self.next();
                Expr::Wildcard
            }

            Token::LParen => {
                self.next();
                let expr = self.parse_expr(0)?;
                self.parse_tokens(&[Token::RParen])?;
                expr
            }

            _ => Err(Unexpected(&token, &location))?,
        };

        Ok(expr)
    }

    fn parse_infix(&mut self, expr: Expr, prec: u8) -> Result<Expr> {
        let TokenWithLocation(token, location) = self.next();
        let op = match token {
            Token::Keyword(kw) => match kw {
                Keyword::And => Some(Op::And),
                Keyword::Or => Some(Op::Or),
                _ => None,
            },
            Token::Eq => Some(Op::Eq),
            Token::Neq => Some(Op::Neq),
            Token::Lt => Some(Op::Lt),
            Token::Le => Some(Op::Le),
            Token::Gt => Some(Op::Gt),
            Token::Ge => Some(Op::Ge),
            _ => None,
        };

        if let Some(op) = op {
            return Ok(Expr::BinaryOp {
                left: Box::new(expr),
                op,
                right: Box::new(self.parse_expr(prec)?),
            });
        }

        let expr = match token {
            Token::Keyword(kw) => match kw {
                Keyword::Is => {
                    let negated = self.check_keywords(&[Keyword::Not]);
                    let TokenWithLocation(token, location) = self.next();
                    match token {
                        Token::Keyword(Keyword::Null) if negated => Expr::IsNotNull(Box::new(expr)),
                        Token::Keyword(Keyword::Null) => Expr::IsNull(Box::new(expr)),
                        _ => Err(Unexpected(&token, &location))?,
                    }
                }
                Keyword::Not | Keyword::Between | Keyword::In => {
                    self.index -= 1;
                    let negated = self.check_keywords(&[Keyword::Not]);
                    if self.check_keywords(&[Keyword::Between]) {
                        self.parse_between(expr, negated)?
                    } else if self.check_keywords(&[Keyword::In]) {
                        self.parse_in(expr, negated)?
                    } else {
                        // Should be the next token?
                        Err(Unexpected(&token, &location))?
                    }
                }
                _ => Err(Unexpected(&token, &location))?,
            },
            _ => Err(Unexpected(&token, &location))?,
        };

        Ok(expr)
    }

    fn next_prec(&self) -> Result<u8> {
        let TokenWithLocation(token, _) = self.peek();
        let prec = match token {
            Token::Eq | Token::Neq | Token::Lt | Token::Le | Token::Gt | Token::Ge => 20,
            Token::Keyword(Keyword::And) => 10,
            Token::Keyword(Keyword::Or) => 5,

            Token::Keyword(Keyword::Not) => {
                let TokenWithLocation(token, location) = self.peek_n(1);
                match token {
                    Token::Keyword(Keyword::Between) => 20,
                    Token::Keyword(Keyword::In) => 20,
                    _ => Err(Unexpected(&token, &location))?,
                }
            }
            Token::Keyword(Keyword::Is) => 17,
            Token::Keyword(Keyword::Between) => 20,
            Token::Keyword(Keyword::In) => 20,
            _ => 0,
        };

        Ok(prec)
    }

    fn parse_value(&mut self) -> Result<Value> {
        let TokenWithLocation(token, location) = self.next();
        match token {
            Token::Keyword(Keyword::False) => Ok(Value::Bool(false)),
            Token::Keyword(Keyword::True) => Ok(Value::Bool(true)),
            Token::Keyword(Keyword::Null) => Ok(Value::Null),
            Token::StringLiteral(s) => Ok(Value::String(s)),
            Token::NumberLiteral(n) => Ok(Value::Number(n)),
            _ => Err(Unexpected(&token, &location))?,
        }
    }

    fn parse_between(&mut self, expr: Expr, negated: bool) -> Result<Expr> {
        let low = self.parse_expr(20)?;
        self.parse_keywords(&[Keyword::And])?;
        let high = self.parse_expr(20)?;

        Ok(Expr::Between {
            expr: Box::new(expr),
            negated,
            low: Box::new(low),
            high: Box::new(high),
        })
    }

    fn parse_in(&mut self, expr: Expr, negated: bool) -> Result<Expr> {
        let mut list = Vec::new();

        self.parse_tokens(&[Token::LParen])?;
        while {
            list.push(self.parse_expr(0)?);
            self.check_tokens(&[Token::Comma])
        } {}
        self.parse_tokens(&[Token::RParen])?;

        Ok(Expr::InList { expr: Box::new(expr), list, negated })
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        let TokenWithLocation(token, location) = self.next();
        let Token::Ident(a) = token else { Err(Unexpected(&token, &location))? };

        let mut parts = Vec::with_capacity(3);
        let ident = if self.check_tokens(&[Token::Dot]) {
            parts.push(a);

            let TokenWithLocation(b, location) = self.next();
            match b {
                Token::Ident(b) => parts.push(b),
                _ => Err(Unexpected(&b, &location))?,
            };

            if self.check_tokens(&[Token::Dot]) {
                let TokenWithLocation(c, location) = self.next();
                match c {
                    Token::Ident(c) => parts.push(c),
                    _ => Err(Unexpected(&c, &location))?,
                };
            }

            Ident::Compound(parts)
        } else {
            Ident::Single(a)
        };

        Ok(ident)
    }

    fn parse_function(&mut self) -> Result<Function> {
        let TokenWithLocation(token, location) = self.next();
        let name = match token {
            Token::Keyword(kw) => match kw {
                Keyword::Min => FunctionName::Min,
                Keyword::Max => FunctionName::Max,
                Keyword::Sum => FunctionName::Sum,
                Keyword::Avg => FunctionName::Avg,
                Keyword::Count => FunctionName::Count,
                _ => Err(Unexpected(&token, &location))?,
            },
            _ => Err(Unexpected(&token, &location))?,
        };

        self.parse_tokens(&[Token::LParen])?;
        let distinct = self.check_keywords(&[Keyword::Distinct]);
        let mut args = Vec::new();
        while {
            args.push(self.parse_expr(0)?);
            self.check_tokens(&[Token::Comma])
        } {}
        self.parse_tokens(&[Token::RParen])?;

        Ok(Function { name, args, distinct })
    }

    // Will advance and return true if tokens match, otherwise walk back and return false
    fn check_tokens(&mut self, tokens: &[Token]) -> bool {
        let index = self.index;

        for want in tokens {
            match self.peek() {
                TokenWithLocation(ref have, ..) if want == have => {
                    self.next();
                    continue;
                }
                _ => {
                    self.index = index;
                    return false;
                }
            }
        }

        true
    }

    fn check_keywords(&mut self, keywords: &[Keyword]) -> bool {
        let index = self.index;

        for want in keywords {
            match self.peek() {
                TokenWithLocation(Token::Keyword(ref have), ..) if want == have => {
                    self.next();
                    continue;
                }
                _ => {
                    self.index = index;
                    return false;
                }
            }
        }

        true
    }

    fn parse_keywords(&mut self, keywords: &[Keyword]) -> Result<()> {
        for want in keywords {
            let TokenWithLocation(token, location) = self.next();
            match token {
                Token::Keyword(ref have) if want == have => continue,
                _ => Err(Unexpected(&token, &location))?,
            }
        }

        Ok(())
    }

    fn parse_tokens(&mut self, tokens: &[Token]) -> Result<()> {
        for want in tokens {
            let TokenWithLocation(ref have, location) = self.next();
            if want == have {
                continue;
            }

            Err(Unexpected(&have, &location))?;
        }

        Ok(())
    }

    fn next(&mut self) -> TokenWithLocation {
        self.index += 1;
        self.get(self.index - 1)
    }

    fn peek(&self) -> TokenWithLocation {
        self.peek_n(0)
    }

    fn peek_n(&self, n: usize) -> TokenWithLocation {
        self.get(self.index + n)
    }

    fn get(&self, i: usize) -> TokenWithLocation {
        self.tokens
            .get(i)
            .map(|t| t.clone())
            .unwrap_or(TokenWithLocation(Token::Eof, Default::default()))
    }
}

#[cfg(test)]
mod test {
    use crate::parser::{FromTable, Join, JoinConstraint, JoinType};

    use super::{
        Assignment, ColumnDef, ColumnType, Create, Delete, Expr, Function, FunctionName, Ident,
        Insert, Op, OrderByExpr, Parser, Query, Select, SelectItem, Statement, Update, Value,
    };

    #[test]
    fn test_create_statement() {
        let input = "
            CREATE TABLE t1 (
                c1 INT,
                c2 VARCHAR(1024)
            )";

        let want = vec![Statement::Create(Create {
            name: Ident::Single("t1".into()),
            columns: vec![
                ColumnDef { ty: ColumnType::Int, name: "c1".into() },
                ColumnDef { ty: ColumnType::Varchar(1024), name: "c2".into() },
            ],
        })];

        let have = Parser::new(input).unwrap().parse().unwrap();
        assert_eq!(want, have)
    }

    #[test]
    fn test_parse_projection() {
        let input = "t1.*, *, s1.t1.c1";

        let want = vec![
            SelectItem::QualifiedWildcard(Ident::Single("t1".into())),
            SelectItem::Wildcard,
            SelectItem::Expr(Expr::Ident(Ident::Compound(vec![
                "s1".into(),
                "t1".into(),
                "c1".into(),
            ]))),
        ];
        let have = Parser::new(input).unwrap().parse_projection().unwrap();
        assert_eq!(want, have)
    }

    macro_rules! test_parse_expr {
        ($name:tt, $input:expr, $want:expr) => {
            #[test]
            fn $name() {
                let mut parser = Parser::new($input).unwrap();
                let have = parser.parse_expr(0).unwrap();
                assert_eq!($want, have);
            }
        };
    }

    test_parse_expr!(
        test_expr_binary_op,
        "c1 < 5",
        Expr::BinaryOp {
            left: Box::new(Expr::Ident(Ident::Single("c1".into()))),
            op: Op::Lt,
            right: Box::new(Expr::Value(Value::Number("5".into()))),
        }
    );

    test_parse_expr!(
        test_expr_binary_op_in,
        "c1 < 5 and c2 in (1, \"2\", 3, \"4\")",
        Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Ident(Ident::Single("c1".into()))),
                op: Op::Lt,
                right: Box::new(Expr::Value(Value::Number("5".into()))),
            }),
            op: Op::And,
            right: Box::new(Expr::InList {
                expr: Box::new(Expr::Ident(Ident::Single("c2".into()))),
                list: vec![
                    Expr::Value(Value::Number("1".into())),
                    Expr::Value(Value::String("2".into())),
                    Expr::Value(Value::Number("3".into())),
                    Expr::Value(Value::String("4".into())),
                ],
                negated: false,
            }),
        }
    );

    test_parse_expr!(
        test_expr_binary_op_not_in,
        "c1 < 5 and c2 not in (1, \"2\", 3, \"4\")",
        Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Ident(Ident::Single("c1".into()))),
                op: Op::Lt,
                right: Box::new(Expr::Value(Value::Number("5".into()))),
            }),
            op: Op::And,
            right: Box::new(Expr::InList {
                expr: Box::new(Expr::Ident(Ident::Single("c2".into()))),
                list: vec![
                    Expr::Value(Value::Number("1".into())),
                    Expr::Value(Value::String("2".into())),
                    Expr::Value(Value::Number("3".into())),
                    Expr::Value(Value::String("4".into())),
                ],
                negated: true,
            }),
        }
    );

    test_parse_expr!(
        test_expr_binary_op_not_in_parens,
        "(c1 < 5) and (c2 not in (1, \"2\", 3, \"4\"))",
        Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Ident(Ident::Single("c1".into()))),
                op: Op::Lt,
                right: Box::new(Expr::Value(Value::Number("5".into()))),
            }),
            op: Op::And,
            right: Box::new(Expr::InList {
                expr: Box::new(Expr::Ident(Ident::Single("c2".into()))),
                list: vec![
                    Expr::Value(Value::Number("1".into())),
                    Expr::Value(Value::String("2".into())),
                    Expr::Value(Value::Number("3".into())),
                    Expr::Value(Value::String("4".into())),
                ],
                negated: true,
            }),
        }
    );

    test_parse_expr!(
        test_expr_parens,
        "c1 < (5 < c2) AND (c1 < 5) < c2",
        Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Ident(Ident::Single("c1".into()))),
                op: Op::Lt,
                right: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Value(Value::Number("5".into()))),
                    op: Op::Lt,
                    right: Box::new(Expr::Ident(Ident::Single("c2".into())))
                })
            }),
            op: Op::And,
            right: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Ident(Ident::Single("c1".into()))),
                    op: Op::Lt,
                    right: Box::new(Expr::Value(Value::Number("5".into())))
                }),
                op: Op::Lt,
                right: Box::new(Expr::Ident(Ident::Single("c2".into())))
            })
        }
    );

    test_parse_expr!(
        test_expr_between,
        "c1 between 0 and 200",
        Expr::Between {
            expr: Box::new(Expr::Ident(Ident::Single("c1".into()))),
            negated: false,
            low: Box::new(Expr::Value(Value::Number("0".into()))),
            high: Box::new(Expr::Value(Value::Number("200".into()))),
        }
    );

    test_parse_expr!(
        test_expr_not_between,
        "c1 not between 0 and 200",
        Expr::Between {
            expr: Box::new(Expr::Ident(Ident::Single("c1".into()))),
            negated: true,
            low: Box::new(Expr::Value(Value::Number("0".into()))),
            high: Box::new(Expr::Value(Value::Number("200".into()))),
        }
    );

    test_parse_expr!(
        test_expr_compound_ident,
        "s1.t1.c1 > 5",
        Expr::BinaryOp {
            left: Box::new(Expr::Ident(Ident::Compound(vec![
                "s1".into(),
                "t1".into(),
                "c1".into()
            ]))),
            op: Op::Gt,
            right: Box::new(Expr::Value(Value::Number("5".into()))),
        }
    );

    test_parse_expr!(
        test_expr_sub_query,
        "1 < (select * from t1 join t2 using (c1) where t1.c2 > t2.c2)",
        Expr::BinaryOp {
            left: Box::new(Expr::Value(Value::Number("1".into()))),
            op: Op::Lt,
            right: Box::new(Expr::SubQuery(Box::new(Query {
                projection: vec![SelectItem::Wildcard],
                from: FromTable::Table { name: Ident::Single("t1".into()), alias: None },
                joins: vec![Join {
                    from: FromTable::Table { name: Ident::Single("t2".into()), alias: None },
                    ty: JoinType::Inner,
                    constraint: JoinConstraint::Using(vec![Ident::Single("c1".into())])
                }],
                filter: Some(Expr::BinaryOp {
                    left: Box::new(Expr::Ident(Ident::Compound(vec!["t1".into(), "c2".into()]))),
                    op: Op::Gt,
                    right: Box::new(Expr::Ident(Ident::Compound(vec!["t2".into(), "c2".into()])))
                }),
                group: vec![]
            }))),
        }
    );

    test_parse_expr!(
        test_expr_is_null,
        "c1 is not null and c2 is null",
        Expr::BinaryOp {
            left: Box::new(Expr::IsNotNull(Box::new(Expr::Ident(Ident::Single("c1".into()))))),
            op: Op::And,
            right: Box::new(Expr::IsNull(Box::new(Expr::Ident(Ident::Single("c2".into()))))),
        }
    );

    #[test]
    fn test_parse_from() {
        let input = "table1 as t1";

        let want =
            FromTable::Table { name: Ident::Single("table1".into()), alias: Some("t1".into()) };
        let have = Parser::new(input).unwrap().parse_from().unwrap();
        assert_eq!(want, have)
    }

    #[test]
    fn test_parse_join() {
        let input = "join t2 on t1.c1 = t2.c1 join t3 using (c2, c3)";

        let want = vec![
            Join {
                from: FromTable::Table { name: Ident::Single("t2".into()), alias: None },
                ty: JoinType::Inner,
                constraint: JoinConstraint::On(Expr::BinaryOp {
                    left: Box::new(Expr::Ident(Ident::Compound(vec!["t1".into(), "c1".into()]))),
                    op: Op::Eq,
                    right: Box::new(Expr::Ident(Ident::Compound(vec!["t2".into(), "c1".into()]))),
                }),
            },
            Join {
                from: FromTable::Table { name: Ident::Single("t3".into()), alias: None },
                ty: JoinType::Inner,
                constraint: JoinConstraint::Using(vec![
                    Ident::Single("c2".into()),
                    Ident::Single("c3".into()),
                ]),
            },
        ];
        let have = Parser::new(input).unwrap().parse_joins().unwrap();
        assert_eq!(want, have)
    }

    #[test]
    fn test_parse_with_derived() {
        let input = "join (select * from t1) t1 using (c1)";

        let want = vec![Join {
            from: FromTable::Derived {
                query: Box::new(Query {
                    projection: vec![SelectItem::Wildcard],
                    from: FromTable::Table { name: Ident::Single("t1".into()), alias: None },
                    joins: vec![],
                    filter: None,
                    group: vec![],
                }),
                alias: Some("t1".into()),
            },
            ty: JoinType::Inner,
            constraint: JoinConstraint::Using(vec![Ident::Single("c1".into())]),
        }];
        let have = Parser::new(input).unwrap().parse_joins().unwrap();
        assert_eq!(want, have)
    }

    #[test]
    fn test_parse_insert() {
        let input = "insert into t1 values ((1, 2), (\"1\", \"2\"))";

        let want = Insert {
            table: Ident::Single("t1".into()),
            rows: vec![
                vec![
                    Expr::Value(Value::Number("1".into())),
                    Expr::Value(Value::Number("2".into())),
                ],
                vec![
                    Expr::Value(Value::String("1".into())),
                    Expr::Value(Value::String("2".into())),
                ],
            ],
        };
        let have = Parser::new(input).unwrap().parse_insert().unwrap();

        assert_eq!(want, have)
    }

    #[test]
    fn test_parse_update() {
        let input = "update t1 set c1 = 1, c2 = \"2\" where 1 = 1";

        let want = Update {
            table: Ident::Single("t1".into()),
            set: vec![
                Assignment {
                    column: Ident::Single("c1".into()),
                    expr: Expr::Value(Value::Number("1".into())),
                },
                Assignment {
                    column: Ident::Single("c2".into()),
                    expr: Expr::Value(Value::String("2".into())),
                },
            ],
            filter: Some(Expr::BinaryOp {
                left: Box::new(Expr::Value(Value::Number("1".into()))),
                op: Op::Eq,
                right: Box::new(Expr::Value(Value::Number("1".into()))),
            }),
        };
        let have = Parser::new(input).unwrap().parse_update().unwrap();

        assert_eq!(want, have)
    }

    #[test]
    fn test_parse_delete() {
        let input = "delete from t1 where 1 = 1";

        let want = Delete {
            table: Ident::Single("t1".into()),
            filter: Some(Expr::BinaryOp {
                left: Box::new(Expr::Value(Value::Number("1".into()))),
                op: Op::Eq,
                right: Box::new(Expr::Value(Value::Number("1".into()))),
            }),
        };
        let have = Parser::new(input).unwrap().parse_delete().unwrap();

        assert_eq!(want, have);
    }

    #[test]
    fn test_parse_select() {
        let input = "select c1, count(distinct *), min(c1) from t1 group by c1 order by c1 limit 5";

        let want = Select {
            body: Query {
                projection: vec![
                    SelectItem::Expr(Expr::Ident(Ident::Single("c1".into()))),
                    SelectItem::Expr(Expr::Function(Function {
                        name: FunctionName::Count,
                        args: vec![Expr::Wildcard],
                        distinct: true,
                    })),
                    SelectItem::Expr(Expr::Function(Function {
                        name: FunctionName::Min,
                        args: vec![Expr::Ident(Ident::Single("c1".into()))],
                        distinct: false,
                    })),
                ],
                from: FromTable::Table { name: Ident::Single("t1".into()), alias: None },
                joins: vec![],
                filter: None,
                group: vec![Expr::Ident(Ident::Single("c1".into()))],
            },
            order: Some(OrderByExpr {
                exprs: vec![Expr::Ident(Ident::Single("c1".into()))],
                desc: false,
            }),
            limit: Some(Expr::Value(Value::Number("5".into()))),
        };
        let have = Parser::new(input).unwrap().parse_select().unwrap();

        assert_eq!(want, have);
    }
}
