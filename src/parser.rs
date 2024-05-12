use crate::tokeniser::{Keyword, Location, Token, TokenWithLocation, Tokeniser, TokeniserError};

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
enum Expr {
    Ident(String),
    CompoundIdent(String),
    Wildcard,
    QualifiedWildcard(Vec<String>),
    Value(Value),
    IsNull(Box<Expr>),
    IsNotNull(Box<Expr>),
    InList { expr: Box<Expr>, list: Vec<Expr>, negated: bool },
    Between { expr: Box<Expr>, negated: bool, low: Box<Expr>, high: Box<Expr> },
    BinaryOp { left: Box<Expr>, op: Op, right: Box<Expr> },
    // TODO: Functions
    // TODO: subquery
}

#[derive(PartialEq, Debug)]
enum FromTable {
    Table { name: Vec<String>, alias: Option<String> },
    Derived { alias: Option<String>, select: Box<Select> },
}

#[derive(PartialEq, Debug)]
struct OrderByExpr {
    expr: Expr,
    desc: bool, // Default is false/ASC
}

#[derive(PartialEq, Debug)]
enum SelectItem {
    Expr(Expr),
    AliasedExpr { expr: Expr, alias: String },
    QualifiedWildcard(Vec<String>),
    Wildcard,
}

#[derive(PartialEq, Debug)]
pub struct Select {
    projection: Vec<SelectItem>,
    from: FromTable,
    joins: Vec<Select>,
    filter: Option<Expr>,
    group: Vec<Expr>,
    order: OrderByExpr,
    limit: Expr,
}

#[derive(PartialEq, Debug)]
pub struct Insert {}

#[derive(PartialEq, Debug)]
pub struct Update {}

#[derive(PartialEq, Debug)]
pub struct Delete {}

#[derive(PartialEq, Debug)]
pub struct Create {
    name: String,
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
            statements.push({
                let TokenWithLocation(token, location) = self.peek();
                match token {
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
                }
            });
        }

        Ok(statements)
    }

    fn parse_select(&mut self) -> Result<Select> {
        self.parse_keywords(&[Keyword::Select])?;

        let projection = self.parse_projection();

        self.parse_keywords(&[Keyword::From])?;

        // parse table and joins

        if self.check_keywords(&[Keyword::Where]) {
            // parse filter
        };

        if self.check_keywords(&[Keyword::Group, Keyword::By]) {
            // parse group
        }

        if self.check_keywords(&[Keyword::Order, Keyword::By]) {
            // parse order
        }

        if self.check_keywords(&[Keyword::Limit]) {
            // parse limit
        }

        Ok(Select {
            projection: todo!(),
            from: todo!(),
            joins: todo!(),
            filter: todo!(),
            group: todo!(),
            order: todo!(),
            limit: todo!(),
        })
    }

    fn parse_insert(&mut self) -> Result<Insert> {
        Ok(Insert {})
    }

    fn parse_update(&mut self) -> Result<Update> {
        Ok(Update {})
    }

    fn parse_delete(&mut self) -> Result<Delete> {
        Ok(Delete {})
    }

    fn parse_create(&mut self) -> Result<Create> {
        self.parse_keywords(&[Keyword::Create, Keyword::Table])?;

        let TokenWithLocation(token, location) = self.next();
        let name = match token {
            Token::Ident(name) => name,
            _ => Err(Unexpected(&token, &location))?,
        };

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
                let mut parts = Vec::with_capacity(2);
                if self.check_tokens(&[Token::Dot]) {
                    parts.push(a);

                    let TokenWithLocation(b, location) = self.next();
                    match b {
                        Token::Ident(b) => parts.push(b),
                        Token::Asterisk => return Ok(SelectItem::QualifiedWildcard(parts)),
                        _ => Err(Unexpected(&b, &location))?,
                    };

                    if self.check_tokens(&[Token::Dot]) {
                        let TokenWithLocation(c, location) = self.next();
                        match c {
                            Token::Ident(_) => {}
                            Token::Asterisk => return Ok(SelectItem::QualifiedWildcard(parts)),
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
            let next_prec = self.next_prec();
            if prec >= next_prec {
                break;
            }

            expr = self.parse_infix()?;
        }

        Ok(expr)
    }

    fn parse_prefix(&mut self) -> Result<Expr> {
        let TokenWithLocation(token, location) = self.next();
        match token {
            Token::Keyword(Keyword::False) => {}
            Token::Keyword(Keyword::True) => {}
            Token::Keyword(Keyword::Null) => {}
            Token::Ident(s) => {
                // try to parse compound
                // wildcard unexpected
            }
            Token::StringLiteral(s) => {}
            Token::NumberLiteral(n) => {}

            _ => Err(Unexpected(&token, &location))?,
        };

        todo!()
    }

    fn parse_infix(&mut self) -> Result<Expr> {
        let TokenWithLocation(token, location) = self.next();
        match token {
            Token::Keyword(_) => todo!(),

            Token::Eq => todo!(),
            Token::Neq => todo!(),
            Token::Lt => todo!(),
            Token::Le => todo!(),
            Token::Gt => todo!(),
            Token::Ge => todo!(),

            _ => todo!(),
        }
    }

    fn next_prec(&self) -> u8 {
        todo!()
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
    use super::{ColumnDef, ColumnType, Create, Parser, SelectItem, Statement};

    #[test]
    fn test_create_statement() {
        let input = "
            CREATE TABLE t1 (
                c1 INT,
                c2 VARCHAR(1024)
            )";

        let want = vec![Statement::Create(Create {
            name: "t1".into(),
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
        let input = "t1.*, *";

        let want = vec![SelectItem::QualifiedWildcard(vec!["t1".into()]), SelectItem::Wildcard];
        let have = Parser::new(input).unwrap().parse_projection().unwrap();
        assert_eq!(want, have)
    }
}
