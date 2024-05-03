use crate::tokeniser::{TokenWithLocation, Tokeniser, TokeniserError};

pub enum Statement {
    Select {},
    Insert {},
    Update {},
    Delete {},
    Create {},
}

pub struct Parser {
    tokens: Vec<TokenWithLocation>,
}

impl Parser {
    pub fn new(src: &str) -> Result<Self, TokeniserError> {
        Tokeniser::new(src).collect_with_location().map(|tokens| Self { tokens })
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        todo!()
    }
}
