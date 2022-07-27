use crate::{ast::SchemeVal, parser::Parser};

struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret_str(src: &str) -> SchemeVal {
        let ast = Parser::new(src).parse().unwrap();

        Self::interpret(ast)
    }

    pub fn interpret(ast: SchemeVal) -> SchemeVal {
        if let SchemeVal::List(list, tail) = ast {
            Self::interpret_call(list)
        } else {
            ast
        }
    }

    fn interpret_call(list: Vec<SchemeVal>) -> SchemeVal {
        let mut iter = list.into_iter();

        let name = if let SchemeVal::Atom(name) = iter.next().unwrap() {
            name
        } else {
            panic!("not a call :(")
        };

        let op = match name.as_str() {
            "+" => |a, b| a + b,
            "-" => |a, b| a - b,
            "*" => |a, b| a * b,
            "/" => |a, b| a / b,
            _ => unimplemented!(),
        };

        let acc = iter
            .map(|n| {
                if let SchemeVal::Number(n) = Self::interpret(n) {
                    n
                } else {
                    panic!("invalid type in arr")
                }
            })
            .reduce(op)
            .unwrap();

        SchemeVal::Number(acc)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_expr() {
        assert_eq!(Interpreter::interpret_str("(+ 9 10 2)"), SchemeVal::Number(21));
    }

    #[test]
    fn nested_expr() {
        assert_eq!(Interpreter::interpret_str("(* (- 10 9) 2)"), SchemeVal::Number(2));
    }
}
