#[derive(Debug, PartialEq, Eq)]
pub enum SchemeVal {
    Atom(String),
    List(Vec<SchemeVal>, Option<Box<SchemeVal>>),
    Number(i64),
    String(String),
    Bool(bool),
}
