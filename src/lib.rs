use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq)]
enum Token {
    Eof,

    // Keywords or Identifiers
    Word(Word),

    // Literals
    StringLiteral(String),
    IntegerLiteral(String),

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
}

#[derive(Debug, PartialEq)]
struct Location {
    line: u64,
    col: u64,
}

#[derive(Debug, PartialEq)]
struct TokenWithLocation(Token, Location);

#[derive(Debug, PartialEq)]
struct Word {
    value: String,
    keyword: Keyword,
}

#[derive(Debug, PartialEq)]
enum Keyword {
    None,

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

impl From<String> for Keyword {
    fn from(s: String) -> Self {
        match s.as_str() {
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

            _ => Keyword::None,
        }
    }
}

struct Tokeniser<'a> {
    src: &'a str,
    chars: Peekable<Chars<'a>>,
    line: u64,
    col: u64,
}

impl<'a> Tokeniser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { src, chars: src.chars().peekable(), line: 0, col: 0 }
    }

    pub fn collect(mut self) -> Vec<Token> {
        let mut v = Vec::new();
        while let Some(t) = self.next() {
            v.push(t)
        }

        v
    }

    pub fn collect_with_location(mut self) -> Vec<TokenWithLocation> {
        let mut v = Vec::new();

        while {
            self.skip_whitespace();
            let loc = self.location();
            match self.next() {
                Some(t) => {
                    v.push(TokenWithLocation(t, loc));
                    true
                }
                None => false,
            }
        } {}

        v
    }

    pub fn location(&self) -> Location {
        Location { line: self.line, col: self.col }
    }

    pub fn next(&mut self) -> Option<Token> {
        if !self.skip_whitespace() {
            return None;
        }

        match self.chars.peek() {
            Some(&c) => match c {
                '0'..='9' => todo!("integer literal"),
                '"' => todo!("string literal"),
                '`' => todo!("quoted identifier"),
                '(' => todo!("lparen"),
                ')' => todo!("rparen"),
                ',' => todo!("comma"),
                '>' => todo!("ge/gt"),
                '<' => todo!("le/lt"),
                '=' => todo!("eq"),
                '!' => todo!("neq"),
                ';' => todo!("semicolon"),
                '*' => todo!("asterisk"),
                ch if ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_' => {
                    // identifier or keyword:

                    let s = self.peeking_take_while(|c| {
                        c.is_alphabetic() || c.is_ascii_digit() || c == '_'
                    });

                    let uppercase_s = s.to_uppercase();
                    Some(Token::Word(Word { value: s, keyword: uppercase_s.into() }))
                }
                ch => unimplemented!("unhandled char: {ch}"),
            },
            None => None,
        }
    }

    /// Skip any whitespace chars, returns true if there are any remaining chars
    fn skip_whitespace(&mut self) -> bool {
        loop {
            match self.peek_char() {
                Some(c) if c.is_whitespace() => {
                    self.next_char();
                    continue;
                }
                Some(c) => match c {
                    '#' => {
                        self.skip_line();
                        continue;
                    }
                    _ => return true,
                },
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
                let have = tokeniser.collect();
                assert_eq!(Vec::from($want), have);
            }
        };
    }

    macro_rules! test_tokeniser_with_location {
        ($name:tt, $input:expr, $want:expr) => {
            #[test]
            fn $name() {
                let tokeniser = Tokeniser::new($input);
                let have = tokeniser.collect_with_location();
                assert_eq!(Vec::from($want), have);
            }
        };
    }

    test_tokeniser!(
        test_select,
        "SELECT",
        [Token::Word(Word { value: "SELECT".into(), keyword: Keyword::Select })]
    );

    test_tokeniser_with_location!(
        test_select_with_location,
        "SELECT",
        [TokenWithLocation(
            Token::Word(Word { value: "SELECT".into(), keyword: Keyword::Select }),
            Location { line: 0, col: 0 }
        )]
    );

    test_tokeniser!(
        test_whitespace,
        "    # This is a comment\n\tSELECT #c2\n#This is another comment\nc1",
        [
            Token::Word(Word { value: "SELECT".into(), keyword: Keyword::Select }),
            Token::Word(Word { value: "c1".into(), keyword: Keyword::None })
        ]
    );

    test_tokeniser_with_location!(
        test_whitespace_with_location,
        "    # This is a comment\n\tSELECT #c2\n#This is another comment\nc1",
        [
            TokenWithLocation(
                Token::Word(Word { value: "SELECT".into(), keyword: Keyword::Select }),
                Location { line: 1, col: 1 }
            ),
            TokenWithLocation(
                Token::Word(Word { value: "c1".into(), keyword: Keyword::None }),
                Location { line: 3, col: 0 }
            )
        ]
    );

    test_tokeniser!(
        test_select_ident_from,
        "SELECT c1 FROM t1",
        [
            Token::Word(Word { value: "SELECT".into(), keyword: Keyword::Select }),
            Token::Word(Word { value: "c1".into(), keyword: Keyword::None }),
            Token::Word(Word { value: "FROM".into(), keyword: Keyword::From }),
            Token::Word(Word { value: "t1".into(), keyword: Keyword::None })
        ]
    );
}
