use std::{
    iter::Peekable,
    str::{CharIndices, Chars},
};

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Token<'a> {
    Eof,

    Keyword(Keyword),
    Ident(&'a str),

    // Literals
    StringLiteral(&'a str),
    NumberLiteral(&'a str),

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

#[derive(Debug, PartialEq, Default, Clone, Copy)]
pub(crate) struct Location {
    line: u64,
    col: u64,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct TokenWithLocation<'a>(pub Token<'a>, pub Location);

#[derive(Debug, PartialEq)]
pub(crate) struct Word {
    value: String,
    keyword: Keyword,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    Not,
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
            "NOT" => Keyword::Not,
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
    src: &'a str,
    chars: Peekable<CharIndices<'a>>,
    line: u64,
    col: u64,
}

impl<'a> Tokeniser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { src, chars: src.char_indices().peekable(), line: 0, col: 0 }
    }

    pub fn collect(mut self) -> Result<Vec<Token<'a>>, TokeniserError> {
        let mut v = Vec::new();
        while {
            let token = self.next()?;
            let eof = token == Token::Eof;
            v.push(token);
            !eof
        } {}

        Ok(v)
    }

    pub fn collect_with_location(mut self) -> Result<Vec<TokenWithLocation<'a>>, TokeniserError> {
        let mut v = Vec::new();
        while {
            let TokenWithLocation(token, location) = self.next_with_location()?;
            let eof = token == Token::Eof;
            v.push(TokenWithLocation(token, location));
            !eof
        } {}

        Ok(v)
    }

    pub fn location(&self) -> Location {
        Location { line: self.line, col: self.col }
    }

    pub fn next_with_location(&mut self) -> Result<TokenWithLocation<'a>, TokeniserError> {
        self.skip_whitespace();
        let loc = self.location();
        self.next().map(|t| TokenWithLocation(t, loc))
    }

    pub fn next(&mut self) -> Result<Token<'a>, TokeniserError> {
        if !self.skip_whitespace() {
            return Ok(Token::Eof);
        }

        match self.peek_char() {
            Some(&(_, c)) => match c {
                '0'..='9' | '.' => {
                    let (int, start, mut end) = self.peeking_take_while(|c| c.is_numeric());

                    if let Some((_, '.')) = self.peek_char() {
                        self.next_char();
                        end += 1;

                        let (dec, dec_start, dec_end) = self.peeking_take_while(|c| c.is_numeric());
                        end += dec_end - dec_start;

                        if int == "" && dec == "" {
                            return Ok(Token::Dot);
                        }
                    }

                    Ok(Token::NumberLiteral(&self.src[start..end]))
                }
                '"' => {
                    self.next_char();
                    let (s, _, _) = self.peeking_take_while(|c| c != '"');
                    match self.next_char() {
                        Some('"') => {}
                        have => Err(TokeniserError::unexpected('"', have, self.location()))?,
                    }

                    Ok(Token::StringLiteral(s))
                }
                '`' => {
                    self.next_char();
                    let (s, _, _) = self.peeking_take_while(|c| {
                        c.is_alphabetic() || c.is_ascii_digit() || c == '_'
                    });
                    match self.next_char() {
                        Some('`') => {}
                        have => Err(TokeniserError::unexpected('`', have, self.location()))?,
                    }

                    Ok(Token::Ident(s))
                }
                '>' => {
                    self.next_char();
                    match self.peek_char() {
                        Some((_, '=')) => self.consume(Token::Ge),
                        _ => Ok(Token::Gt),
                    }
                }
                '<' => {
                    self.next_char();
                    match self.peek_char() {
                        Some((_, '=')) => self.consume(Token::Le),
                        _ => Ok(Token::Lt),
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
                ch if ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_' => {
                    // identifier or keyword:

                    let (s, _, _) = self.peeking_take_while(|c| {
                        c.is_alphabetic() || c.is_ascii_digit() || c == '_'
                    });

                    match Keyword::try_from(s.to_uppercase()) {
                        Ok(kw) => Ok(Token::Keyword(kw)),
                        _ => Ok(Token::Ident(s)),
                    }
                }
                ch => unimplemented!("unhandled char: {ch}"),
            },
            None => Ok(Token::Eof),
        }
    }

    fn consume(&mut self, t: Token<'a>) -> Result<Token<'a>, TokeniserError> {
        self.next_char();
        Ok(t)
    }

    /// Skip any whitespace chars, returns true if there are any remaining chars
    fn skip_whitespace(&mut self) -> bool {
        loop {
            match self.peek_char() {
                Some((_, c)) if c.is_whitespace() => {
                    self.next_char();
                    continue;
                }
                Some((_, '#')) => {
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
                Some(&(_, c)) => {
                    self.next_char();
                    if c == '\n' {
                        break;
                    }
                    continue;
                }
                None => return false,
            }
        }

        self.peek_char().is_some()
    }

    fn peek_char(&mut self) -> Option<&(usize, char)> {
        self.chars.peek()
    }

    fn next_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some((_, c)) => {
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

    /// Returns the string, the start index in the src, and the end index in the src
    fn peeking_take_while(
        &mut self,
        mut predicate: impl FnMut(char) -> bool,
    ) -> (&'a str, usize, usize) {
        let Some(&(mut start, _)) = self.peek_char() else { return ("", 0, 0) };
        let mut end = start;
        while let Some(&(_, c)) = self.peek_char() {
            if predicate(c) {
                self.next_char();
                end += 1;
            } else {
                break;
            }
        }

        (&self.src[start..end], start, end)
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

    test_tokeniser!(test_select, "SELECT", [Token::Keyword(Keyword::Select), Token::Eof]);

    test_tokeniser_with_location!(
        test_select_with_location,
        "SELECT",
        [
            TokenWithLocation(Token::Keyword(Keyword::Select), Location { line: 0, col: 0 }),
            TokenWithLocation(Token::Eof, Location { line: 0, col: 6 })
        ]
    );

    test_tokeniser!(
        test_whitespace,
        "    # This is a comment\n\tSELECT #c2\n#This is another comment\nc1",
        [Token::Keyword(Keyword::Select), Token::Ident("c1"), Token::Eof]
    );

    test_tokeniser_with_location!(
        test_whitespace_with_location,
        "    # This is a comment\n\tSELECT #c2\n#This is another comment\nc1",
        [
            TokenWithLocation(Token::Keyword(Keyword::Select), Location { line: 1, col: 1 }),
            TokenWithLocation(Token::Ident("c1"), Location { line: 3, col: 0 }),
            TokenWithLocation(Token::Eof, Location { line: 3, col: 2 })
        ]
    );

    test_tokeniser!(
        test_select_ident_from,
        "SELECT c1 FROM t1",
        [
            Token::Keyword(Keyword::Select),
            Token::Ident("c1"),
            Token::Keyword(Keyword::From),
            Token::Ident("t1"),
            Token::Eof
        ]
    );

    test_tokeniser!(
        test_select_multi_ident_from,
        "SELECT s1.t1.c1, c2 FROM s1.t1",
        [
            Token::Keyword(Keyword::Select),
            Token::Ident("s1"),
            Token::Dot,
            Token::Ident("t1"),
            Token::Dot,
            Token::Ident("c1"),
            Token::Comma,
            Token::Ident("c2"),
            Token::Keyword(Keyword::From),
            Token::Ident("s1"),
            Token::Dot,
            Token::Ident("t1"),
            Token::Eof
        ]
    );

    test_tokeniser!(
        test_select_int_and_float,
        "SELECT 1, 2.34, 5., .5",
        [
            Token::Keyword(Keyword::Select),
            Token::NumberLiteral("1"),
            Token::Comma,
            Token::NumberLiteral("2.34"),
            Token::Comma,
            Token::NumberLiteral("5."),
            Token::Comma,
            Token::NumberLiteral(".5"),
            Token::Eof
        ]
    );

    test_tokeniser!(
        test_select_where,
        "SELECT * FROM t1 WHERE a < b OR b <= c OR c > d OR d >= e",
        [
            Token::Keyword(Keyword::Select),
            Token::Asterisk,
            Token::Keyword(Keyword::From),
            Token::Ident("t1"),
            Token::Keyword(Keyword::Where),
            Token::Ident("a"),
            Token::Lt,
            Token::Ident("b"),
            Token::Keyword(Keyword::Or),
            Token::Ident("b"),
            Token::Le,
            Token::Ident("c"),
            Token::Keyword(Keyword::Or),
            Token::Ident("c"),
            Token::Gt,
            Token::Ident("d"),
            Token::Keyword(Keyword::Or),
            Token::Ident("d"),
            Token::Ge,
            Token::Ident("e"),
            Token::Eof
        ]
    );

    test_tokeniser!(
        test_select_string,
        "SELECT \"c1\"",
        [Token::Keyword(Keyword::Select), Token::StringLiteral("c1"), Token::Eof]
    );

    test_tokeniser!(
        test_select_multi_line_string,
        "SELECT \"c1
2
3\"",
        [Token::Keyword(Keyword::Select), Token::StringLiteral("c1\n2\n3"), Token::Eof]
    );

    test_tokeniser!(
        test_select_quoted_ident,
        "SELECT `s1`.`t1`",
        [
            Token::Keyword(Keyword::Select),
            Token::Ident("s1"),
            Token::Dot,
            Token::Ident("t1"),
            Token::Eof
        ]
    );
}
