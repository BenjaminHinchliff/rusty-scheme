use std::{collections::HashMap, ops, rc::Rc};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::SchemeVal,
    parser::{ParseError, Parser},
    types::SchemeType,
};

fn option_schemeval_to_string(val: &Option<Rc<SchemeVal>>) -> String {
    val.as_ref().map(|g| g.to_string()).unwrap_or_default()
}

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
    #[error("Expected atom in define - given {given}")]
    ExpectedAtom { given: SchemeVal },
    #[error(
        "Invalid type in call to {function} - given {} but expected {expected}",
        option_schemeval_to_string(given)
    )]
    InvalidType {
        function: String,
        given: Option<Rc<SchemeVal>>,
        expected: SchemeType,
    },
    #[error(
        "Expected function call but was given {}",
        option_schemeval_to_string(given)
    )]
    ExpectedFunctionCall { given: Option<Rc<SchemeVal>> },
}

pub type InterpretResult<T> = Result<T, InterpretError>;

type Value = Rc<SchemeVal>;

#[derive(Clone, EnumAsInner)]
enum Definition {
    External(fn(&[&SchemeVal]) -> SchemeVal),
    Value(Option<Value>),
}

pub struct Interpreter {
    symbols: Vec<HashMap<String, Definition>>,
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

    pub fn interpret_str(&mut self, src: &str) -> InterpretResult<Option<Value>> {
        let ast = Parser::new(src).parse()?;

        self.interpret(ast)
    }

    pub fn interpret(&mut self, ast: Vec<SchemeVal>) -> InterpretResult<Option<Value>> {
        let mut symbol = None;
        for val in ast {
            symbol = self.interpret_one(val)?;
        }
        Ok(symbol)
    }

    pub fn interpret_one(&mut self, ast: SchemeVal) -> InterpretResult<Option<Value>> {
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
                .map(|s| s.as_scheme().unwrap()) // TODO: prevent crash when trying to read external symbol
                .cloned(),
        }
    }

    fn add_symbol(&mut self, name: String, definition: Definition) {
        self.symbols.last_mut().unwrap().insert(name, definition);
    }

    fn get_symbol(&self, name: &str) -> InterpretResult<&Definition> {
        self.symbols
            .last()
            .unwrap()
            .get(name)
            .ok_or_else(|| InterpretError::UnboundSymbol {
                symbol: name.to_string(),
            })
    }

    fn interpret_call(&mut self, list: Vec<SchemeVal>) -> InterpretResult<Option<Value>> {
        let mut iter = list.into_iter();

        let name = iter
            .next()
            .ok_or(InterpretError::ExpectedFunctionCall { given: None })?
            .into_atom()
            .map_err(|e| InterpretError::ExpectedFunctionCall {
                given: Some(Rc::new(e)),
            })?;

        if name == "define" {
            let symbol = iter
                .next()
                .ok_or(InterpretError::ImproperArgCount {
                    function: "define".to_string(),
                    given: 0,
                    expected: 2,
                })?
                .into_atom()
                .map_err(|e| InterpretError::ExpectedAtom { given: e })?;
            let definition = self.interpret_one(iter.next().ok_or_else(|| {
                InterpretError::ImproperArgCount {
                    function: "define".to_string(),
                    given: 1,
                    expected: 2,
                }
            })?)?;

            self.add_symbol(symbol, Definition::Value(definition));
            Ok(None)
        } else {
            let op = match name.as_str() {
                "+" => ops::Add::add,
                "-" => ops::Sub::sub,
                "*" => ops::Mul::mul,
                "/" => ops::Div::div,
                _ => unimplemented!(),
            };

            let acc = iter
                .map(|n| -> InterpretResult<_> {
                    let n = match n {
                        SchemeVal::Atom(sym) => self
                            .get_symbol(&sym)
                            .map(|s| s.as_scheme().unwrap())
                            .cloned(),
                        n => self.interpret_one(n),
                    }?
                    .ok_or_else(|| InterpretError::InvalidType {
                        function: name.clone(),
                        given: None,
                        expected: SchemeType::Number,
                    })?;
                    n.clone()
                        .as_number()
                        .ok_or_else(|| InterpretError::InvalidType {
                            function: name.clone(),
                            given: Some(n),
                            expected: SchemeType::Number,
                        })
                        .copied()
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
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

    fn check_err(src: &str, err: InterpretError) {
        let actual = Interpreter::new().interpret_str(src).unwrap_err();
        assert_eq!(actual, err);
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

    #[test]
    fn err_unbound_symbol() {
        check_err(
            "two",
            InterpretError::UnboundSymbol {
                symbol: "two".to_string(),
            },
        )
    }

    #[test]
    fn err_expected_function_call() {
        check_err("()", InterpretError::ExpectedFunctionCall { given: None });
        check_err(
            "(2)",
            InterpretError::ExpectedFunctionCall {
                given: Some(Rc::new(SchemeVal::Number(2))),
            },
        );
    }

    #[test]
    fn err_() {}
}
