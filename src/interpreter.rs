use std::{
    collections::HashMap,
    fmt::{self, Display},
    ops,
};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::SchemeVal,
    parser::{ParseError, Parser},
    types::SchemeType,
};

fn option_to_string(val: &Option<impl ToString>) -> String {
    val.as_ref().map(|g| g.to_string()).unwrap_or_default()
}

#[derive(Debug, PartialEq, thiserror::Error)]
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
        option_to_string(given)
    )]
    InvalidType {
        function: String,
        given: Option<Symbol>,
        expected: SchemeType,
    },
    #[error(
        "Expected function call but was given {}",
        option_to_string(given)
    )]
    ExpectedFunctionCall { given: Option<SchemeVal> },
}

pub type InterpretResult<T> = Result<T, InterpretError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<SchemeType>,
}

#[derive(Clone)]
pub enum Function {
    External(Prototype, fn(&[&SchemeVal]) -> SchemeVal),
    Scheme(Prototype, Box<Symbol>),
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::External(proto, _) => write!(f, "External({:?})", proto),
            Function::Scheme(proto, body) => write!(f, "Scheme({:?}, {:?})", proto, body),
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Function::External(s_proto, _), Function::External(o_proto, _)) => s_proto == o_proto,
            (Function::Scheme(s_proto, s_body), Function::Scheme(o_proto, o_body)) => {
                s_proto == o_proto && s_body == o_body
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum Symbol {
    Value(SchemeVal),
    Function(Function),
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Value(v) => write!(f, "{}", v),
            Symbol::Function(_func) => write!(f, "function"),
        }
    }
}

pub struct Interpreter {
    variables: Vec<HashMap<String, Option<Symbol>>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: vec![HashMap::new()],
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
            s @ SchemeVal::String(_) => Ok(Some(Symbol::Value(s))),
            n @ SchemeVal::Number(_) => Ok(Some(Symbol::Value(n))),
            b @ SchemeVal::Bool(_) => Ok(Some(Symbol::Value(b))),
            SchemeVal::List(mut list, tail) => {
                if let Some(SchemeVal::Atom(atom)) = list.first() {
                    if atom == "quote" {
                        list.remove(0);
                        return Ok(Some(Symbol::Value(SchemeVal::List(list, tail))));
                    }
                }

                self.interpret_call(list)
            }
            SchemeVal::Atom(sym) => self.get_symbol(&sym).cloned(),
        }
    }

    fn add_symbol(&mut self, name: String, definition: Option<Symbol>) {
        self.variables.last_mut().unwrap().insert(name, definition);
    }

    fn get_symbol(&self, name: &str) -> InterpretResult<&Option<Symbol>> {
        self.variables
            .last()
            .unwrap()
            .get(name)
            .ok_or_else(|| InterpretError::UnboundSymbol {
                symbol: name.to_string(),
            })
    }

    fn interpret_call(&mut self, list: Vec<SchemeVal>) -> InterpretResult<Option<Symbol>> {
        let mut iter = list.into_iter();

        let name = iter
            .next()
            .ok_or(InterpretError::ExpectedFunctionCall { given: None })?
            .into_atom()
            .map_err(|e| InterpretError::ExpectedFunctionCall { given: Some(e) })?;

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
            let definition = if let Some(n) = iter.next() {
                // TODO: find a way to use .map() here?
                self.interpret_one(n)?
            } else {
                None
            };

            self.add_symbol(symbol, definition);
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
                        SchemeVal::Atom(sym) => self.get_symbol(&sym).cloned(),
                        n => self.interpret_one(n),
                    }?
                    .ok_or_else(|| InterpretError::InvalidType {
                        function: name.clone(),
                        given: None,
                        expected: SchemeType::Number,
                    })?;
                    n.clone()
                        .as_value()
                        .ok_or_else(|| InterpretError::InvalidType {
                            function: name.clone(),
                            given: Some(n.clone()),
                            expected: SchemeType::Number,
                        })?
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

            Ok(Some(Symbol::Value(SchemeVal::Number(acc))))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(src: &str, expect: Option<Symbol>) {
        let actual = Interpreter::new().interpret_str(src).unwrap();
        assert_eq!(actual, expect);
    }

    fn check_err(src: &str, err: InterpretError) {
        let actual = Interpreter::new().interpret_str(src).unwrap_err();
        assert_eq!(actual, err);
    }

    #[test]
    fn basic_expr() {
        check("(+ 9 10 2)", Some(Symbol::Value(SchemeVal::Number(21))));
    }

    #[test]
    fn nested_expr() {
        check(
            "(* (- 10 8) (+ 10 9))",
            Some(Symbol::Value(SchemeVal::Number(38))),
        );
    }

    #[test]
    fn define() {
        let src = r#"
            (define two 2)
            two
        "#;

        check(src, Some(Symbol::Value(SchemeVal::Number(2))));
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
                given: Some(SchemeVal::Number(2)),
            },
        );
    }

    #[test]
    fn err_() {}
}
