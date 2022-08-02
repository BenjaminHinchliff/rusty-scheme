use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SchemeType {
    Number,
    String,
    Bool,
}

impl fmt::Display for SchemeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                SchemeType::Number => "number",
                SchemeType::String => "string",
                SchemeType::Bool => "bool",
            }
        )
    }
}
