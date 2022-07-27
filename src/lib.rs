mod ast;
mod lexer;
mod parser;
mod interpreter;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
