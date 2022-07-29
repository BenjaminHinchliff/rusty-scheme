use std::fmt;

use enum_as_inner::EnumAsInner;

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum SchemeVal {
    Atom(String),
    List(Vec<SchemeVal>, Option<Box<SchemeVal>>),
    Number(i64),
    String(String),
    Bool(bool),
}

impl fmt::Display for SchemeVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn list_to_string(list: &[SchemeVal]) -> String {
            if let Some(first) = list.first() {
                let mut acc = first.to_string();

                for v in &list[1..] {
                    acc += " ";
                    acc += &v.to_string();
                }
                acc
            } else {
                String::new()
            }
        }

        match self {
            SchemeVal::Atom(s) => write!(f, "Atom@{}", s),
            SchemeVal::List(l, t) => write!(
                f,
                "List@({}{})",
                list_to_string(l),
                if let Some(t) = t {
                    format!(" . {}", t)
                } else {
                    String::new()
                }
            ),
            SchemeVal::Number(n) => write!(f, "Number@{}", n),
            SchemeVal::String(s) => write!(f, "String@{}", s),
            SchemeVal::Bool(b) => write!(f, "Bool@{}", b),
        }
    }
}
