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
pub struct  EffectDeclaration {
    pub name: Effect,
    pub in_types: Vec<Type>,
    pub out_type: Option<Type>,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct FunDeclaration {
    pub name: Identifier,
    pub arguments: Vec<TypedIdentifier>,
    pub out_type: Option<Type>,
    pub expression: Box<Expression>,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct HandlerDeclaration {
    pub name: Identifier,
    pub arguments: Vec<TypedIdentifier>,
    pub out_type: Option<Type>,
    pub return_handler: Option<ReturnHandlerDeclaration>,
    pub effect_handlers: Vec<EffectHandlerDeclaration>,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct ReturnHandlerDeclaration {
    pub ret_arg: TypedIdentifier,
    pub expression: Box<Expression>,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct EffectHandlerDeclaration {
    pub effect: Effect,
    pub arguments: Vec<TypedIdentifier>,
    pub expression: Box<Expression>,
}