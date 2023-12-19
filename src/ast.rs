use std::{collections::HashSet, hash::Hash, fmt::Display};

mod types;
mod operators;
mod declarations;

pub use types::*;
pub use operators::*;
pub use declarations::*;

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum Value {
    ULiteral,
    BLiteral(bool),
    I32Literal(i32),
    Var(Identifier),
    RuneLiteral(char),
    StringLiteral(String),
    //Fun(Identifier),
    //NativeFun(Identifier),
    //Handler(Identifier),
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum Expression {
    Value( Box<Value> ),
    Sequencing( Box<Expression>, Box<Expression> ),
    Let{ id: Identifier, expression: Box<Expression> },
    If{ guard: Box<Expression>, then_b: Box<Expression>, else_b: Box<Expression> },
    While{ guard: Box<Expression>, block: Box<Expression> },
    ValueCall{ expression: Box<Expression> },
    FunCall{ function: Identifier, arguments: Vec<Expression> },
    EffCall{ effect: Effect, arguments: Vec<Expression> },
    Handling{ handler: Identifier, computation: Box<Expression> },

    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier(value.to_owned())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}