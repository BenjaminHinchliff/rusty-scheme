use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::SchemeVal,
    parser::{ParseError, Parser},
};

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum InterpretError {
    #[error("Parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("Failed to define {symbol} - {error}")]
    DefineError { symbol: String, error: String },
    #[error("Unbound symbol: {symbol}")]
    UnboundSymbol { symbol: String },
    #[error("Improper arg count in call to {function} - given {given} but expected {expected}")]
    ImproperArgCount {
        function: String,
        given: usize,
        expected: usize,
    },
    #[error(
        "Expected function call but was given {}",
        given.as_ref().map(|g| g.to_string()).unwrap_or_else(|| String::from("()"))
    )]
    ExpectedFunctionCall { given: Option<SchemeVal> },
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
        let ast = Parser::new(src).parse()?;

        self.interpret(ast)
    }

    pub fn interpret(&mut self, ast: Vec<SchemeVal>) -> InterpretResult<Option<Symbol>> {
        let mut symbol = None;
        for val in ast {
            symbol = self.interpret_one(val)?;
        }
        Ok(symbol)
    }

    pub fn interpret_one(&mut self, ast: SchemeVal) -> InterpretResult<Option<Symbol>> {
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

        let name = iter
            .next()
            .ok_or(InterpretError::ExpectedFunctionCall { given: None })?
            .into_atom()
            .map_err(|e| InterpretError::ExpectedFunctionCall { given: Some(e) })?;

        if name == "define" {
            let symbol = iter.next().unwrap().into_atom().unwrap();
            let definition = self.interpret_one(iter.next().unwrap())?.unwrap();

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
                        n => self.interpret_one(n)?.unwrap(),
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

    fn check(src: &str, expect: Option<SchemeVal>) {
        let actual = Interpreter::new().interpret_str(src).unwrap();
        assert_eq!(actual, expect.map(Rc::new));
    }

    #[test]
    fn basic_expr() {
        check("(+ 9 10 2)", Some(SchemeVal::Number(21)));
    }

    #[test]
    fn nested_expr() {
        check("(* (- 10 8) (+ 10 9))", Some(SchemeVal::Number(38)));
    }

    #[test]
    fn define() {
        let src = r#"
            (define two 2)
            two
        "#;

        check(src, Some(SchemeVal::Number(2)));
    }
}
