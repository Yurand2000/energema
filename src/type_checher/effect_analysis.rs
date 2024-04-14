use std::collections::{HashSet, HashMap};

use crate::ast::*;

use super::*;

pub struct EffectAnalyzer<'a> {
    effects: HashSet<Effect>,
    functions: HashMap<Identifier, ExpressionEffects>,
    handler: HashMap<Identifier, HandlerEffects>,
    declarations: &'a [Declaration],
}

#[derive(Debug)]
#[derive(Default)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
struct ExpressionEffects {
    performed_effects: Vec<Effect>,
    nested_effects: Vec<NestedEffects>
}

struct HandlerEffects {
    return_effects: Vec<Effect>,
    handler_effects: HashMap<Effect, ExpressionEffects>
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
enum NestedEffects {
    FunCall(Identifier),
    Handler(Identifier, Box<ExpressionEffects>),
}

type EffectEnvironment = Environment<ExpressionEffects>;

impl EffectAnalyzer<'_> {
    pub fn new(declarations: &'_ [Declaration]) -> Self {
        let effects = Self::get_effects_names(declarations);
        Self { effects, functions: todo!(), handler: todo!(), declarations }
    }

    fn get_effects_names(declarations: &'_ [Declaration]) -> HashSet<Effect> {
        let mut set = HashSet::new();

        for declaration in declarations {
            match declaration {
                Declaration::Effect(effect) => { set.insert(effect.name.clone()); },
                _ => (),
            }
        }

        set
    }

    fn get_effects(expr: Expression, env: &mut EffectEnvironment) -> ExpressionEffects {
        match expr {
            Expression::Value(_) => ExpressionEffects::default(),
            Expression::VarValue(_) => ExpressionEffects::default(),
            Expression::Sequencing(left, right) => {
                let mut left_effects = Self::get_effects(*left, env);
                let right_effects = Self::get_effects(*right, env);
                left_effects.join(right_effects);
                left_effects
            },
            Expression::Let { id, expression } => {
                let effects = Self::get_effects(*expression, env);
                env.new_identifier(&id, effects.clone());
                effects
            },
            Expression::If { guard, then_b, else_b } => {
                let mut guard_effects = Self::get_effects(*guard, env);
                let then_b_effects = Self::get_effects_in_block(*then_b, env);
                let else_b_effects = Self::get_effects_in_block(*else_b, env);
                guard_effects.join(then_b_effects);
                guard_effects.join(else_b_effects);
                guard_effects
            },
            Expression::While { guard, block } => {
                let mut guard_effects = Self::get_effects(*guard, env);
                let block_effects = Self::get_effects_in_block(*block, env);
                guard_effects.join(block_effects);
                guard_effects
            },
            Expression::FunCall { function, arguments } => {
                let mut effects = ExpressionEffects::default();
                if let Expression::VarValue(id) = *function {
                    effects.nested_effects.push(NestedEffects::FunCall(id));
                } else {
                    effects = Self::get_effects(*function, env);
                }

                for argument in arguments {
                    let argument_effects = Self::get_effects(argument, env);
                    effects.join(argument_effects);
                }

                effects
            },
            Expression::EffCall { effect, arguments } => {
                let mut effects = ExpressionEffects::default();
                effects.performed_effects.push(effect);
                for argument in arguments {
                    let argument_effects = Self::get_effects(argument, env);
                    effects.join(argument_effects);
                }

                effects
            },
            Expression::Handling { handler, arguments, computation } => {
                env.push_handler(todo!());
                let effects = Self::get_effects(*computation, env);
                env.pop_handler();

                let out_effects = ExpressionEffects::default();
                out_effects.nested_effects.push( NestedEffects::Handler(handler, Box::new(effects)) );
                out_effects                
            },
            Expression::ClosureCreate { arguments, closure: computation } => ExpressionEffects::default(),
            Expression::Block(expr) => Self::get_effects_in_block(*expr, env),
            Expression::UnaryOp(_, expr) => Self::get_effects(*expr, env),
            Expression::BinaryOp(lexpr, _, rexpr) => {
                let mut left_effects = Self::get_effects(*lexpr, env);
                let right_effects = Self::get_effects(*rexpr, env);
                left_effects.join(right_effects);
                left_effects
            },
        }
    }

    fn get_effects_in_block(expr: Expression, env: &mut EffectEnvironment) -> ExpressionEffects {
        env.push_block();
        let effects = Self::get_effects(expr, env);
        env.pop_block();
        effects
    }
}

impl ExpressionEffects {
    pub fn join(&mut self, other: Self) {
        self.performed_effects.extend(other.performed_effects.into_iter());
        self.nested_effects.extend(other.nested_effects.into_iter());
    }
}