pub use super::*;

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum Type {
    Void,
    Unit,
    Bool,
    I32,
    Rune,
    String,
    Computation(Box<ComputationType>),
    Fun(Box<FunctionType>),
    Handler(Box<HandlerType>),
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct FunctionType {
    pub arguments: Vec<Type>,
    pub out_type: ComputationType
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct HandlerType {
    pub arguments: Vec<Type>,
    pub in_type: Type,
    pub out_type: Type,
    pub managed_effects: Vec<Effect>,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct ComputationType {
    pub typ: Type,
    pub effects: ComputationEffects,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum ComputationEffects {
    AtMost(HashSet<Effect>),
    AllBut(HashSet<Effect>),
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq, Hash)]
pub struct Effect {
    pub eff_type: Identifier,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct TypedIdentifier {
    pub id: Identifier,
    pub typ: Type,
}

impl TypedIdentifier {
    pub fn new(id: Identifier, typ: Type) -> Self {
        Self { id, typ }
    }
}