use std::{collections::HashMap, rc::Rc};

use crate::{ast::SchemeVal, parser::Parser};

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum InterpretError {
    #[error("Failed to define {symbol} - {error}")]
    DefineError { symbol: String, error: String },
}

pub type InterpretResult<T> = Result<T, InterpretError>;

#[derive(Debug, Default)]
pub struct Interpreter {
    symbols: HashMap<String, Rc<SchemeVal>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn interpret_str(&mut self, src: &str) -> InterpretResult<Rc<SchemeVal>> {
        let ast = Parser::new(src).parse().unwrap();

        self.interpret(ast)
    }

    pub fn interpret(&mut self, ast: SchemeVal) -> InterpretResult<Rc<SchemeVal>> {
        match ast {
            s @ SchemeVal::String(_) => Ok(Rc::new(s)),
            n @ SchemeVal::Number(_) => Ok(Rc::new(n)),
            b @ SchemeVal::Bool(_) => Ok(Rc::new(b)),
            SchemeVal::List(mut list, tail) => {
                if let Some(SchemeVal::Atom(atom)) = list.first() {
                    if atom == "quote" {
                        list.remove(0);
                        return Ok(Rc::new(SchemeVal::List(list, tail)));
                    }
                }

                self.interpret_call(list)
            }
            _ => unimplemented!(),
        }
    }

    fn interpret_call(&mut self, list: Vec<SchemeVal>) -> InterpretResult<Rc<SchemeVal>> {
        let mut iter = list.into_iter();

        let name = iter.next().unwrap().into_atom().unwrap();

        if name == "define" {
            let symbol = iter.next().unwrap().into_atom().unwrap();
            let definition = self.interpret(iter.next().unwrap())?;

            self.symbols.insert(symbol, definition.clone());
            Ok(definition)
        } else {
            let op = match name.as_str() {
                "+" => |a, b| a + b,
                "-" => |a, b| a - b,
                "*" => |a, b| a * b,
                "/" => |a, b| a / b,
                _ => unimplemented!(),
            };

            let acc = iter
                .map(|n| -> InterpretResult<_> {
                    Ok(match n {
                        SchemeVal::Atom(sym) => self.symbols.get(&sym).unwrap().clone(),
                        n => self.interpret(n)?,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(|n| n.as_number().copied().unwrap())
                .reduce(op)
                .unwrap();

            Ok(Rc::new(SchemeVal::Number(acc)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_expr() {
        assert_eq!(
            Interpreter::new().interpret_str("(+ 9 10 2)").unwrap(),
            Rc::new(SchemeVal::Number(21))
        );
    }

    #[test]
    fn nested_expr() {
        assert_eq!(
            Interpreter::new().interpret_str("(* (- 10 9) 2)").unwrap(),
            Rc::new(SchemeVal::Number(2))
        );
    }

    #[test]
    fn def_test() {
        let mut interpreter = Interpreter::new();
        assert_eq!(
            interpreter.interpret_str("(define two 2)").unwrap(),
            Rc::new(SchemeVal::Number(2))
        );
        assert_eq!(
            interpreter.interpret_str("(+ two 2)").unwrap(),
            Rc::new(SchemeVal::Number(4))
        );
    }
}
