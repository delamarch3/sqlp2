use crate::tokeniser::{Keyword, Location, Token, TokenWithLocation, Tokeniser, TokeniserError};

pub enum Statement {
    Select(Select),
    Insert(Insert),
    Update(Update),
    Delete(Delete),
    Create(Create),
}

pub struct Select {}
pub struct Insert {}
pub struct Update {}
pub struct Delete {}
pub struct Create {
    table_name: String,
}

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

        let TokenWithLocation(token, location) = self.next();
        let table_name = match token {
            Token::Ident(name) => name,
            _ => Err(Unexpected(&token, &location))?,
        };

        self.parse_tokens(&[Token::LParen])?;

        // TODO: Parse column defs

        Ok(Create { table_name })
    }

    fn parse_keywords(&mut self, keywords: &[Keyword]) -> Result<()> {
        for want in keywords {
            let TokenWithLocation(token, location) = self.next();
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
                TokenWithLocation(have, location) if want == &have => continue,
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
