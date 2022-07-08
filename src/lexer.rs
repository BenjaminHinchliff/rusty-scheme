use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[regex(r";.*\n|[ \t\n\f]+")]
    Whitespace,

    #[regex(r"[a-zA-Z!$%&*+\-./:<=>?@^_~]+")]
    Identifier,

    #[token("(")]
    Open,

    #[token(")")]
    Close,

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
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, kind: Token) {
        let mut lexer = Token::lexer(input);

        assert_eq!(lexer.next(), Some(kind));
        assert_eq!(lexer.slice(), input);
    }

    #[test]
    fn spaces() {
        check("  ", Token::Whitespace);
        check("\n\n", Token::Whitespace);
        check("\t\t", Token::Whitespace);
    }

    #[test]
    fn ident() {
        check("hello-world?", Token::Identifier);
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
}
