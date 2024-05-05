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
pub struct Select {}
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
        Ok(Select {})
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

        let name = match self.next() {
            TokenWithLocation(token, location) => match token {
                Token::Ident(name) => name,
                _ => Err(Unexpected(&token, &location))?,
            },
        };

        self.parse_tokens(&[Token::LParen])?;
        let mut columns = Vec::new();
        while {
            columns.push(self.parse_column_def()?);
            self.check_token(Token::Comma)
        } {}
        self.parse_tokens(&[Token::RParen])?;

        Ok(Create { name, columns })
    }

    fn parse_column_def(&mut self) -> Result<ColumnDef> {
        let name = match self.next() {
            TokenWithLocation(token, location) => match token {
                Token::Ident(name) => name,
                _ => Err(Unexpected(&token, &location))?,
            },
        };

        let ty = match self.next() {
            TokenWithLocation(token, location) => match token {
                Token::Keyword(Keyword::Int) => ColumnType::Int,
                Token::Keyword(Keyword::Varchar) => {
                    self.parse_tokens(&[Token::LParen])?;
                    let max: u16 = match self.next() {
                        TokenWithLocation(token, location) => match token {
                            Token::NumberLiteral(ref max) => {
                                max.parse().map_err(|_| Unexpected(&token, &location))?
                            }
                            _ => Err(Unexpected(&token, &location))?,
                        },
                    };
                    self.parse_tokens(&[Token::RParen])?;
                    ColumnType::Varchar(max)
                }
                _ => Err(Unexpected(&token, &location))?,
            },
        };

        Ok(ColumnDef { ty, name })
    }

    fn check_token(&mut self, want: Token) -> bool {
        match self.peek() {
            TokenWithLocation(have, ..) if want == have => {
                self.next();
                true
            }
            _ => false,
        }
    }

    fn parse_keywords(&mut self, keywords: &[Keyword]) -> Result<()> {
        for want in keywords {
            match self.next() {
                TokenWithLocation(Token::Keyword(have), ..) if want == &have => continue,
                TokenWithLocation(token, location) => Err(Unexpected(&token, &location))?,
            }
        }

        Ok(())
    }

    fn parse_tokens(&mut self, tokens: &[Token]) -> Result<()> {
        for want in tokens {
            match self.next() {
                TokenWithLocation(have, ..) if want == &have => continue,
                TokenWithLocation(token, location) => Err(Unexpected(&token, &location))?,
            }
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
    use super::{ColumnDef, ColumnType, Create, Parser, Statement};

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
}
