pub use super::*;

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum Declaration {
    Function(FunDeclaration),
    Handler(HandlerDeclaration),
    Effect(EffectDeclaration),
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct FunDeclaration {
    pub name: Identifier,
    pub arguments: Vec<Identifier>,
    pub expression: Box<Expression>,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct HandlerDeclaration {
    pub name: Identifier,
    pub return_handler: Option<(Type, Type, Box<Expression>)>,
    pub effect_handlers: Vec<(Effect, Vec<Identifier>, Box<Expression>)>,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct  EffectDeclaration {
    pub name: Identifier,
    pub in_types: Vec<Type>,
    pub out_type: Type,
}