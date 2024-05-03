use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq)]
pub(crate) enum Token {
    Eof,

    Keyword(Keyword),
    Ident(String),

    // Literals
    StringLiteral(String),
    NumberLiteral(String),

    // Operators
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,

    // Symbols
    LParen,
    RParen,
    Semicolon,
    Comma,
    Asterisk,
    Dot,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Location {
    line: u64,
    col: u64,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct TokenWithLocation(Token, Location);

#[derive(Debug, PartialEq)]
pub(crate) struct Word {
    value: String,
    keyword: Keyword,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Keyword {
    Int,
    Varchar,

    Create,
    Table,
    Select,
    Insert,
    Update,
    Delete,
    Into,
    Values,
    From,
    Where,
    Join,
    On,
    Using,
    As,
    Limit,
    And,
    Or,
    Negation,
    Null,
    In,
    Between,
    Is,
    Group,
    By,
    Order,
    Asc,
    Desc,
    Set,
    True,
    False,
}

impl TryFrom<String> for Keyword {
    type Error = ();

    fn try_from(s: String) -> Result<Self, Self::Error> {
        let kw = match s.as_str() {
            "AND" => Keyword::And,
            "AS" => Keyword::As,
            "ASC" => Keyword::Asc,
            "BETWEEN" => Keyword::Between,
            "BY" => Keyword::By,
            "CREATE" => Keyword::Create,
            "DELETE" => Keyword::Delete,
            "DESC" => Keyword::Desc,
            "FALSE" => Keyword::False,
            "FROM" => Keyword::From,
            "GROUP" => Keyword::Group,
            "IN" => Keyword::In,
            "INSERT" => Keyword::Insert,
            "INT" => Keyword::Int,
            "INTO" => Keyword::Into,
            "IS" => Keyword::Is,
            "JOIN" => Keyword::Join,
            "LIMIT" => Keyword::Limit,
            "NOT" => Keyword::Negation,
            "NULL" => Keyword::Null,
            "ON" => Keyword::On,
            "OR" => Keyword::Or,
            "SELECT" => Keyword::Select,
            "SET" => Keyword::Set,
            "TABLE" => Keyword::Table,
            "TRUE" => Keyword::True,
            "UPDATE" => Keyword::Update,
            "USING" => Keyword::Using,
            "VALUES" => Keyword::Values,
            "VARCHAR" => Keyword::Varchar,
            "WHERE" => Keyword::Where,
            "ORDER" => Keyword::Order,

            _ => Err(())?,
        };

        Ok(kw)
    }
}

#[derive(Debug)]
pub enum TokeniserError {
    Unexpected { want: char, have: char, location: Location },
}

impl std::fmt::Display for TokeniserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokeniserError::Unexpected { want, have, location } => {
                write!(f, "{}: unexpected char, want: {}, have: {}", location, want, have)
            }
        }
    }
}

impl std::error::Error for TokeniserError {}

impl TokeniserError {
    fn unexpected(want: char, have: Option<char>, location: Location) -> Self {
        Self::Unexpected { want, have: have.unwrap_or(' '), location }
    }
}

pub(crate) struct Tokeniser<'a> {
    chars: Peekable<Chars<'a>>,
    line: u64,
    col: u64,
}

impl<'a> Tokeniser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { chars: src.chars().peekable(), line: 0, col: 0 }
    }

    pub fn collect(mut self) -> Result<Vec<Token>, TokeniserError> {
        let mut v = Vec::new();
        while let Some(t) = self.next()? {
            v.push(t)
        }

        Ok(v)
    }

    pub fn collect_with_location(mut self) -> Result<Vec<TokenWithLocation>, TokeniserError> {
        let mut v = Vec::new();
        while let Some(t) = self.next_with_location()? {
            v.push(t)
        }

        Ok(v)
    }

    pub fn location(&self) -> Location {
        Location { line: self.line, col: self.col }
    }

    pub fn next_with_location(&mut self) -> Result<Option<TokenWithLocation>, TokeniserError> {
        self.skip_whitespace();
        let loc = self.location();
        self.next().map(|o| o.map(|t| TokenWithLocation(t, loc)))
    }

    pub fn next(&mut self) -> Result<Option<Token>, TokeniserError> {
        if !self.skip_whitespace() {
            return Ok(None);
        }

        match self.peek_char() {
            Some(&c) => match c {
                '0'..='9' | '.' => {
                    let mut s = self.peeking_take_while(|c| c.is_numeric());

                    if let Some('.') = self.peek_char() {
                        self.next_char();
                        s.push('.');
                        s.push_str(&self.peeking_take_while(|c| c.is_numeric()))
                    }

                    if s == "." {
                        return Ok(Some(Token::Dot));
                    }

                    Ok(Some(Token::NumberLiteral(s)))
                }
                '"' => {
                    self.next_char();
                    let s = self.peeking_take_while(|c| c != '"');
                    match self.next_char() {
                        Some('"') => {}
                        have => Err(TokeniserError::unexpected('"', have, self.location()))?,
                    }

                    Ok(Some(Token::StringLiteral(s)))
                }
                '`' => {
                    self.next_char();
                    let s = self.peeking_take_while(|c| {
                        c.is_alphabetic() || c.is_ascii_digit() || c == '_'
                    });
                    match self.next_char() {
                        Some('`') => {}
                        have => Err(TokeniserError::unexpected('`', have, self.location()))?,
                    }

                    Ok(Some(Token::Ident(s)))
                }
                '>' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('=') => self.consume(Token::Ge),
                        _ => Ok(Some(Token::Gt)),
                    }
                }
                '<' => {
                    self.next_char();
                    match self.peek_char() {
                        Some('=') => self.consume(Token::Le),
                        _ => Ok(Some(Token::Lt)),
                    }
                }
                '!' => {
                    self.next_char();
                    match self.next_char() {
                        Some('=') => self.consume(Token::Neq),
                        have => Err(TokeniserError::unexpected('`', have, self.location()))?,
                    }
                }
                '=' => self.consume(Token::Eq),
                '(' => self.consume(Token::LParen),
                ')' => self.consume(Token::RParen),
                ',' => self.consume(Token::Comma),
                ';' => self.consume(Token::Semicolon),
                '*' => self.consume(Token::Asterisk),
                // TODO: two different conditons 🤔
                ch if ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_' => {
                    // identifier or keyword:

                    let s = self.peeking_take_while(|c| {
                        c.is_alphabetic() || c.is_ascii_digit() || c == '_'
                    });

                    match Keyword::try_from(s.to_uppercase()) {
                        Ok(kw) => Ok(Some(Token::Keyword(kw))),
                        _ => Ok(Some(Token::Ident(s))),
                    }
                }
                ch => unimplemented!("unhandled char: {ch}"),
            },
            None => Ok(None),
        }
    }

    fn consume(&mut self, t: Token) -> Result<Option<Token>, TokeniserError> {
        self.next_char();
        Ok(Some(t))
    }

    /// Skip any whitespace chars, returns true if there are any remaining chars
    fn skip_whitespace(&mut self) -> bool {
        loop {
            match self.peek_char() {
                Some(c) if c.is_whitespace() => {
                    self.next_char();
                    continue;
                }
                Some('#') => {
                    self.skip_line();
                    continue;
                }
                Some(_) => return true,
                None => return false,
            }
        }
    }

    /// Skip a line, returns true if there are any remaining chars
    fn skip_line(&mut self) -> bool {
        loop {
            match self.peek_char() {
                Some(c) => {
                    if *c == '\n' {
                        break;
                    }

                    self.next_char();
                    continue;
                }
                None => return false,
            }
        }

        self.next_char();
        self.peek_char().is_some()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn next_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                if c == '\n' {
                    self.col = 0;
                    self.line += 1;
                } else {
                    self.col += 1;
                }

                Some(c)
            }
            None => None,
        }
    }

    fn peeking_take_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> String {
        let mut s = String::new();
        while let Some(&c) = self.peek_char() {
            if predicate(c) {
                self.next_char();
                s.push(c);
            } else {
                break;
            }
        }

        s
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_tokeniser {
        ($name:tt, $input:expr, $want:expr) => {
            #[test]
            fn $name() {
                let tokeniser = Tokeniser::new($input);
                let have = tokeniser.collect().unwrap();
                assert_eq!(Vec::from($want), have);
            }
        };
    }

    macro_rules! test_tokeniser_with_location {
        ($name:tt, $input:expr, $want:expr) => {
            #[test]
            fn $name() {
                let tokeniser = Tokeniser::new($input);
                let have = tokeniser.collect_with_location().unwrap();
                assert_eq!(Vec::from($want), have);
            }
        };
    }

    test_tokeniser!(test_select, "SELECT", [Token::Keyword(Keyword::Select)]);

    test_tokeniser_with_location!(
        test_select_with_location,
        "SELECT",
        [TokenWithLocation(Token::Keyword(Keyword::Select), Location { line: 0, col: 0 })]
    );

    test_tokeniser!(
        test_whitespace,
        "    # This is a comment\n\tSELECT #c2\n#This is another comment\nc1",
        [Token::Keyword(Keyword::Select), Token::Ident("c1".into())]
    );

    test_tokeniser_with_location!(
        test_whitespace_with_location,
        "    # This is a comment\n\tSELECT #c2\n#This is another comment\nc1",
        [
            TokenWithLocation(Token::Keyword(Keyword::Select), Location { line: 1, col: 1 }),
            TokenWithLocation(Token::Ident("c1".into()), Location { line: 3, col: 0 })
        ]
    );

    test_tokeniser!(
        test_select_ident_from,
        "SELECT c1 FROM t1",
        [
            Token::Keyword(Keyword::Select),
            Token::Ident("c1".into()),
            Token::Keyword(Keyword::From),
            Token::Ident("t1".into())
        ]
    );

    test_tokeniser!(
        test_select_multi_ident_from,
        "SELECT s1.t1.c1, c2 FROM s1.t1",
        [
            Token::Keyword(Keyword::Select),
            Token::Ident("s1".into()),
            Token::Dot,
            Token::Ident("t1".into()),
            Token::Dot,
            Token::Ident("c1".into()),
            Token::Comma,
            Token::Ident("c2".into()),
            Token::Keyword(Keyword::From),
            Token::Ident("s1".into()),
            Token::Dot,
            Token::Ident("t1".into()),
        ]
    );

    test_tokeniser!(
        test_select_int_and_float,
        "SELECT 1, 2.34, 5., .5",
        [
            Token::Keyword(Keyword::Select),
            Token::NumberLiteral("1".into()),
            Token::Comma,
            Token::NumberLiteral("2.34".into()),
            Token::Comma,
            Token::NumberLiteral("5.".into()),
            Token::Comma,
            Token::NumberLiteral(".5".into()),
        ]
    );

    test_tokeniser!(
        test_select_where,
        "SELECT * FROM t1 WHERE a < b OR b <= c OR c > d OR d >= e",
        [
            Token::Keyword(Keyword::Select),
            Token::Asterisk,
            Token::Keyword(Keyword::From),
            Token::Ident("t1".into()),
            Token::Keyword(Keyword::Where),
            Token::Ident("a".into()),
            Token::Lt,
            Token::Ident("b".into()),
            Token::Keyword(Keyword::Or),
            Token::Ident("b".into()),
            Token::Le,
            Token::Ident("c".into()),
            Token::Keyword(Keyword::Or),
            Token::Ident("c".into()),
            Token::Gt,
            Token::Ident("d".into()),
            Token::Keyword(Keyword::Or),
            Token::Ident("d".into()),
            Token::Ge,
            Token::Ident("e".into()),
        ]
    );

    test_tokeniser!(
        test_select_string,
        "SELECT \"c1\"",
        [Token::Keyword(Keyword::Select), Token::StringLiteral("c1".into())]
    );

    test_tokeniser!(
        test_select_multi_line_string,
        "SELECT \"c1
2
3\"",
        [Token::Keyword(Keyword::Select), Token::StringLiteral("c1\n2\n3".into())]
    );

    test_tokeniser!(
        test_select_quoted_ident,
        "SELECT `s1`.`t1`",
        [
            Token::Keyword(Keyword::Select),
            Token::Ident("s1".into()),
            Token::Dot,
            Token::Ident("t1".into())
        ]
    );
}
