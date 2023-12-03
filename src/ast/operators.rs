
#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum UnaryOp {
    LNot,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    LAnd, LOr, LXor,
    Eq, Ne, Gt, Ge, Lt, Le,
}