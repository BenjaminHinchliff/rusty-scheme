use std::fmt;

use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq)]
pub enum Token {
    #[regex(r"[a-zA-Z!$%&*+\-./:<=>?@^_~][a-zA-Z0-9!$%&*+\-./:<=>?@^_~]*")]
    Atom,

    #[token("(")]
    Open,

    #[token(")")]
    Close,

    #[token(".")]
    Dot,

    // https://regex101.com/r/k6qXng
    // string regex that allows for forward shash character escapes and
    // doesn't crash
    #[regex(r#""(?s:[^"\\]|\\.)*""#)]
    String,

    #[regex("[0-9]+")]
    Number,

    #[regex("#t|true|#f|false")]
    Bool,

    #[error]
    #[regex(r";.*\n|[ \t\n\f]+", logos::skip)]
    Error,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Atom => "atom",
                Token::Open => "(",
                Token::Close => ")",
                Token::Dot => ".",
                Token::String => "string",
                Token::Number => "number",
                Token::Bool => "bool",
                Token::Error => "error token",
            }
        )
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: Token::lexer(input),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        Some((kind, text))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, kind: Token) {
        let mut lexer = Token::lexer(input);

        assert_eq!(lexer.next(), Some(kind));
        assert_eq!(lexer.slice(), input);
    }

    fn check_many(input: &str, kinds: &[Token]) {
        let lexed: Vec<_> = Token::lexer(input).collect();

        assert_eq!(lexed, kinds);
    }

    #[test]
    fn atom() {
        check("hello-world?", Token::Atom);
    }

    #[test]
    fn open_paren() {
        check("(", Token::Open);
    }

    #[test]
    fn close_paren() {
        check(")", Token::Close);
    }

    #[test]
    fn string() {
        check("\"test string and stuff\"", Token::String);
    }

    #[test]
    fn number() {
        check("1234567890", Token::Number);
    }

    #[test]
    fn bool() {
        check("#t", Token::Bool);
        check("#f", Token::Bool);
        check("true", Token::Bool);
        check("false", Token::Bool);
    }

    #[test]
    fn full_expr() {
        check_many(
            "(+ 2 3)",
            &[
                Token::Open,
                Token::Atom,
                Token::Number,
                Token::Number,
                Token::Close,
            ],
        )
    }

    #[test]
    fn nested() {
        check_many(
            "(+ (- 10 5) 2)",
            &[
                Token::Open,
                Token::Atom,
                Token::Open,
                Token::Atom,
                Token::Number,
                Token::Number,
                Token::Close,
                Token::Number,
                Token::Close,
            ],
        )
    }
}
