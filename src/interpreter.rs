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
    #[error("Argument to function cannot be None")]
    ExpectedValue,
    #[error(
        "Invalid type in call to {function} - given {} but expected {expected}",
        option_to_string(given)
    )]
    InvalidType {
        function: String,
        given: Option<Symbol>,
        expected: SchemeType,
    },
    #[error("Expected function call but was given {}", option_to_string(given))]
    ExpectedFunctionCall { given: Option<Symbol> },
}

pub type InterpretResult = Result<Option<Symbol>, InterpretError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemeFunction {
    bindings: Vec<String>,
    body: SchemeVal,
}

#[derive(Clone)]
pub enum Function {
    External(fn(Vec<Symbol>) -> InterpretResult),
    Scheme(SchemeFunction),
}

impl Function {
    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Symbol>) -> InterpretResult {
        match self {
            Function::External(ext) => ext(args),
            Function::Scheme(SchemeFunction { bindings, body }) => {
                interpreter.push_scope();
                for (binding, arg) in bindings.iter().zip(args) {
                    interpreter.add_symbol(binding.clone(), Some(arg));
                }
                let result = interpreter.interpret_one(body.clone());
                interpreter.pop_scope();
                result
            }
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::External(_) => write!(f, "External(?)"),
            Function::Scheme(func) => write!(f, "Scheme({:?})", func),
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Function::External(_), Function::External(_)) => true,
            (Function::Scheme(s), Function::Scheme(o)) => s == o,
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

fn scheme_binop(name: String, op: fn(i64, i64) -> i64, args: Vec<Symbol>) -> InterpretResult {
    Ok(Some(Symbol::Value(SchemeVal::Number(
        args.into_iter()
            .map(|s| {
                s.into_value()
                    .map_err(|e| InterpretError::InvalidType {
                        function: name.clone(),
                        given: Some(e),
                        expected: SchemeType::Number,
                    })?
                    .into_number()
                    .map_err(|e| InterpretError::InvalidType {
                        function: name.clone(),
                        given: Some(Symbol::Value(e)),
                        expected: SchemeType::Number,
                    })
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .reduce(op)
            .unwrap_or(0),
    ))))
}

macro_rules! add_binop {
    ($globals:expr, $name:expr, $func:expr) => {
        $globals.insert(
            $name.to_string(),
            Some(Symbol::Function(Function::External(|args| {
                scheme_binop($name.to_string(), $func, args)
            }))),
        )
    };
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
        let mut globals = HashMap::new();
        add_binop!(globals, "+", ops::Add::add);
        add_binop!(globals, "-", ops::Sub::sub);
        add_binop!(globals, "*", ops::Mul::mul);
        add_binop!(globals, "/", ops::Div::div);
        globals.insert(
            "=".to_string(),
            Some(Symbol::Function(Function::External(|args| {
                if args.len() != 2 {
                    return Err(InterpretError::ImproperArgCount {
                        function: "=".to_string(),
                        given: args.len(),
                        expected: 2,
                    });
                }

                let mut args = args
                    .iter()
                    .map(|a| *a.as_value().unwrap().as_number().unwrap());

                let first = args.next().unwrap();

                Ok(Some(Symbol::Value(SchemeVal::Bool(
                    args.all(|a| first == a),
                ))))
            }))),
        );
        globals.insert(
            "if".to_string(),
            Some(Symbol::Function(Function::External(|args| {
                if args.len() < 2 || args.len() > 3 {
                    return Err(InterpretError::ImproperArgCount {
                        function: "if".to_string(),
                        given: args.len(),
                        expected: 2,
                    });
                }

                let cond = *args[0].as_value().unwrap().as_bool().unwrap();

                Ok(args.into_iter().nth(if cond { 1 } else { 2 }))
            }))),
        );

        Self {
            variables: vec![globals],
        }
    }

    // doesn't compile due to implicit casting not working properly
    // fn add_binop(&mut self, name: &str, operation: fn(i64, i64) -> i64) {
    //     self.add_symbol(
    //         name.to_string(),
    //         Some(Symbol::Function(Function::External(|args| {
    //             scheme_binop(name.to_string(), operation, args)
    //         }))),
    //     )
    // }

    pub fn interpret_str(&mut self, src: &str) -> InterpretResult {
        let ast = Parser::new(src).parse()?;

        self.interpret(ast)
    }

    pub fn interpret(&mut self, ast: Vec<SchemeVal>) -> InterpretResult {
        let mut symbol = None;
        for val in ast {
            symbol = self.interpret_one(val)?;
        }
        Ok(symbol)
    }

    pub fn interpret_one(&mut self, ast: SchemeVal) -> InterpretResult {
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

    fn push_scope(&mut self) {
        self.variables.push(HashMap::new())
    }

    fn pop_scope(&mut self) {
        self.variables.pop();
    }

    fn add_symbol(&mut self, name: String, definition: Option<Symbol>) {
        self.variables.last_mut().unwrap().insert(name, definition);
    }

    fn get_symbol(&self, name: &str) -> Result<&Option<Symbol>, InterpretError> {
        for scope in self.variables.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Ok(symbol);
            }
        }
        Err(InterpretError::UnboundSymbol {
            symbol: name.to_string(),
        })
    }

    fn interpret_call(&mut self, list: Vec<SchemeVal>) -> InterpretResult {
        let mut iter = list.into_iter();

        let name = iter
            .next()
            .ok_or(InterpretError::ExpectedFunctionCall { given: None })?
            .into_atom()
            .map_err(|e| InterpretError::ExpectedFunctionCall {
                given: Some(Symbol::Value(e)),
            })?;

        match &name[..] {
            "define" => {
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
            }
            "lambda" => {
                let bindings: Vec<_> = iter
                    .next()
                    .unwrap()
                    .into_list()
                    .unwrap()
                    .0
                    .into_iter()
                    .map(|b| b.into_atom().unwrap())
                    .collect();

                let body = iter.next().unwrap();
                Ok(Some(Symbol::Function(Function::Scheme(SchemeFunction {
                    bindings,
                    body,
                }))))
            }
            _ => {
                let func = self
                    .get_symbol(&name)?
                    .as_ref()
                    .ok_or(InterpretError::UnboundSymbol { symbol: name })?
                    .clone();
                let func = func
                    .as_function()
                    .ok_or(InterpretError::ExpectedFunctionCall { given: None })?;

                let args = iter
                    .map(|n| {
                        match n {
                            SchemeVal::Atom(sym) => self.get_symbol(&sym).cloned(),
                            n => self.interpret_one(n),
                        }?
                        .ok_or(InterpretError::ExpectedValue)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                func.call(self, args)
            }
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
    fn lambda() {
        let src = r#"
            (define times-two (lambda (n) (* n 2)))
            (times-two 8)
        "#;

        check(src, Some(Symbol::Value(SchemeVal::Number(16))));
    }

    #[test]
    fn if_statement() {
        let src = r#"
            (define one? (lambda (n) (if (= n 1) 1 0)))
            (one? 1)
        "#;

        check(src, Some(Symbol::Value(SchemeVal::Number(1))));
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
                given: Some(Symbol::Value(SchemeVal::Number(2))),
            },
        );
    }
}
