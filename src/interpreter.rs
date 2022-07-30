use std::{collections::HashMap, rc::Rc};

use crate::{ast::SchemeVal, parser::Parser};

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum InterpretError {
    #[error("Failed to define {symbol} - {error}")]
    DefineError { symbol: String, error: String },
    #[error("Unbound symbol: {symbol}")]
    UnboundSymbol { symbol: String },
}

pub type InterpretResult<T> = Result<T, InterpretError>;

type Symbol = Rc<SchemeVal>;

#[derive(Debug)]
pub struct Interpreter {
    symbols: Vec<HashMap<String, Rc<SchemeVal>>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            symbols: vec![HashMap::new()],
        }
    }

    pub fn interpret_str(&mut self, src: &str) -> InterpretResult<Option<Symbol>> {
        let ast = Parser::new(src).parse().unwrap();

        self.interpret(ast)
    }

    pub fn interpret(&mut self, ast: SchemeVal) -> InterpretResult<Option<Symbol>> {
        match ast {
            s @ SchemeVal::String(_) => Ok(Some(Rc::new(s))),
            n @ SchemeVal::Number(_) => Ok(Some(Rc::new(n))),
            b @ SchemeVal::Bool(_) => Ok(Some(Rc::new(b))),
            SchemeVal::List(mut list, tail) => {
                if let Some(SchemeVal::Atom(atom)) = list.first() {
                    if atom == "quote" {
                        list.remove(0);
                        return Ok(Some(Rc::new(SchemeVal::List(list, tail))));
                    }
                }

                self.interpret_call(list)
            }
            SchemeVal::Atom(sym) => self
                .get_symbol(&sym)
                .map(|sym| Some(sym.clone()))
                .ok_or(InterpretError::UnboundSymbol { symbol: sym }),
        }
    }

    fn add_symbol(&mut self, name: String, definition: Symbol) {
        self.symbols.last_mut().unwrap().insert(name, definition);
    }

    fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.last().unwrap().get(name)
    }

    fn interpret_call(&mut self, list: Vec<SchemeVal>) -> InterpretResult<Option<Symbol>> {
        let mut iter = list.into_iter();

        let name = iter.next().unwrap().into_atom().unwrap();

        if name == "define" {
            let symbol = iter.next().unwrap().into_atom().unwrap();
            let definition = self.interpret(iter.next().unwrap())?.unwrap();

            self.add_symbol(symbol, definition);
            Ok(None)
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
                        SchemeVal::Atom(sym) => self.get_symbol(&sym).unwrap().clone(),
                        n => self.interpret(n)?.unwrap(),
                    })
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(|n| n.as_number().copied().unwrap())
                .reduce(op)
                .unwrap();

            Ok(Some(Rc::new(SchemeVal::Number(acc))))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check(src: &str, expect: Expect) {
        let actual = Interpreter::new()
            .interpret_str(src)
            .unwrap()
            .map(|v| v.to_string())
            .unwrap_or_else(String::new);
        expect.assert_eq(&actual)
    }

    #[test]
    fn basic_expr() {
        check("(+ 9 10 2)", expect!["21"]);
    }

    #[test]
    fn nested_expr() {
        assert_eq!(
            Interpreter::new().interpret_str("(* (- 10 9) 2)").unwrap(),
            Some(Rc::new(SchemeVal::Number(2)))
        );
    }

    #[test]
    fn def_test() {
        let mut interpreter = Interpreter::new();
        assert_eq!(interpreter.interpret_str("(define two 2)").unwrap(), None);
        assert_eq!(
            interpreter.interpret_str("(+ two 2)").unwrap(),
            Some(Rc::new(SchemeVal::Number(4)))
        );
    }
}
