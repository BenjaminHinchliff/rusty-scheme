use std::iter::Peekable;

use crate::{
    ast::SchemeVal,
    lexer::{Lexer, Token},
};

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ParseError {
    #[error("unexpected end of file")]
    UnexpectedEOF,
    #[error("expected {expected} found {given}")]
    UnexpectedToken { given: Token, expected: Token },
    #[error("failed to lex all tokens")]
    FailedLexing,
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
        }
    }

    fn peek(&mut self) -> ParseResult<&(Token, &str)> {
        self.lexer.peek().ok_or(ParseError::UnexpectedEOF)
    }

    fn next(&mut self) -> ParseResult<(Token, &str)> {
        self.lexer.next().ok_or(ParseError::UnexpectedEOF)
    }

    fn check(&mut self, token: Token) -> ParseResult<()> {
        let (next, _) = self.next()?;

        if next == token {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                given: next,
                expected: token,
            })
        }
    }

    pub fn parse(&mut self) -> ParseResult<SchemeVal> {
        let (token, _) = self.peek()?;

        match token {
            Token::String => self.parse_string(),
            Token::Bool => self.parse_bool(),
            Token::Atom => self.parse_atom(),
            Token::Number => self.parse_number(),
            Token::Open => self.parse_list(),
            Token::Error => Err(ParseError::FailedLexing),
            _ => unreachable!(),
        }
    }

    fn parse_number(&mut self) -> ParseResult<SchemeVal> {
        let (token, slice) = self.next()?;
        debug_assert_eq!(token, Token::Number);

        Ok(SchemeVal::Number(slice.parse().unwrap()))
    }

    fn parse_atom(&mut self) -> ParseResult<SchemeVal> {
        let (token, slice) = self.next()?;
        debug_assert_eq!(token, Token::Atom);

        Ok(SchemeVal::Atom(slice.to_string()))
    }

    fn parse_list(&mut self) -> ParseResult<SchemeVal> {
        let (token, _) = self.next()?;
        debug_assert_eq!(token, Token::Open);

        let mut list = Vec::new();
        loop {
            list.push(self.parse()?);

            let next = &self.peek()?.0;

            if next == &Token::Close || next == &Token::Dot {
                break;
            }
        }

        let tail = if self.peek()?.0 == Token::Dot {
            self.next()?;
            let tail = self.parse()?;
            Some(Box::new(tail))
        } else {
            None
        };

        self.check(Token::Close)?;

        Ok(SchemeVal::List(list, tail))
    }

    fn parse_string(&mut self) -> ParseResult<SchemeVal> {
        let (token, slice) = self.next()?;
        debug_assert_eq!(token, Token::String);

        let string = &slice[1..slice.len() - 1];

        Ok(SchemeVal::String(string.to_string()))
    }

    fn parse_bool(&mut self) -> ParseResult<SchemeVal> {
        let (token, slice) = self.next()?;
        debug_assert_eq!(token, Token::Bool);

        Ok(SchemeVal::Bool(match slice {
            "#t" | "true" => true,
            "#f" | "false" => false,
            _ => unreachable!(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check(src: &str, expect: Expect) {
        let actual = Parser::new(src).parse().unwrap().to_string();
        expect.assert_eq(&actual);
    }

    #[test]
    fn basic_expr() {
        check("(+ 2 3)", expect!["(+ 2 3)"]);
    }

    #[test]
    fn nested_operations() {
        check("(* (- 10 9) 2)", expect!["(* (- 10 9) 2)"]);
    }

    #[test]
    fn dotted_list() {
        check("(1 2 . 3)", expect!["(1 2 . 3)"]);
    }

    #[test]
    fn define() {
        check("(define two 2)", expect!["(define two 2)"]);
    }
}
