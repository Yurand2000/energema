use std::collections::HashMap;

use crate::ast::*;

mod effect_analysis;
mod utils;

use effect_analysis::*;
use utils::*;

pub struct TypeChecker<'a> {
    effects: HashMap<Effect, EffectDeclaration>,
    functions: HashMap<Identifier, FunctionType>,
    handlers: HashMap<Identifier, HandlerType>,
    declarations: &'a [Declaration],
}

impl TypeChecker<'_> {
    pub fn new(declarations: &'_ [Declaration]) -> Self {
        Self{ effects: todo!(), functions: todo!(), handlers: todo!(), declarations }
    }

    fn get_effects(declarations: &'_ [Declaration]) -> HashMap<Effect, EffectDeclaration> {
        let mut map = HashMap::new();
        for declaration in declarations.iter() {
            match declaration {
                Declaration::Effect(effect) => {
                    map.insert(effect.name.clone(), effect.clone());
                },
                _ => (),
            }
        }

        map
    }

    fn get_handlers(declarations: &'_ [Declaration]) -> HashMap<Identifier, HandlerType> {
        let mut map = HashMap::new();
        for declaration in declarations.iter() {
            match declaration {
                Declaration::Handler(handler) => {
                    let arguments = handler.arguments.iter().map(|typed_id| typed_id.typ.clone() ).collect();
                    let out_type = handler.out_type.clone().unwrap_or(Type::Unit);
                    let in_type = handler.return_handler.as_ref().map_or(out_type.clone(), |handler| {
                            handler.ret_arg.typ.clone()
                        });

                    let managed_effects = handler.effect_handlers.iter().map(|handler| {
                            handler.effect.clone()
                        }).collect();

                    let typ = HandlerType {
                        arguments,
                        in_type,
                        out_type,
                        managed_effects,
                    };

                    map.insert(handler.name.clone(), typ);
                }
                _ => (),
            }
        }

        map
    }

    fn get_functions(declarations: &'_ [Declaration]) -> HashMap<Identifier, FunctionType> {
        let mut map = HashMap::new();
        for declaration in declarations.iter() {
            match declaration {
                Declaration::Function(function) => {
                    let out_type = ComputationType {
                        typ: function.out_type.clone().unwrap_or(Type::Unit),
                        effects: todo!(),
                    };

                    let typ = FunctionType {
                        arguments: todo!(),
                        out_type,
                    };

                    map.insert(function.name, typ);
                },
                _ => (),
            }
        }

        map
    }
}