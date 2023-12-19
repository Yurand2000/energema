use std::collections::{HashMap, HashSet};

use super::ast::*;

mod interpreter_data;
use interpreter_data::*;

#[derive(Clone)]
pub struct Declarations {
    functions: HashMap<Identifier, FunDeclaration>,
    handlers: HashMap<Identifier, HandlerDeclaration>,
    effects: HashMap<Identifier, EffectDeclaration>,

    native_functions: HashMap<Identifier, NativeFun>
}

struct Environment {
    declarations: Declarations,
    stacks: Vec<EnvBlock>,
    expression: IExpression,
}

impl Environment {
    pub fn new(declarations: Declarations, expression: Expression) -> Self {
        Self { declarations, stacks: Vec::new(), expression: expression.into() }
    }

    pub fn next(mut self) -> Result<Self, String> {
        self.expression = Self::interpret_next(&self.declarations, self.expression, &mut self.stacks)?;
        Ok(self)
    }

    pub fn has_next(&self) -> bool {
        if let IExpression::Value(_) = &self.expression { false } else { true }
    }

    pub fn finish(mut self) -> Result<IValue, String> {
        while self.has_next() {
            self = self.next()?;
        }

        let IExpression::Value(val) = self.expression else { panic!("unexpected panic! [0]") };
        Ok(*val)
    }

    fn interpret_next(defs: &Declarations, expr: IExpression, stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        match expr {
            IExpression::Value(value) =>
                Self::interpret_value(defs, value, stacks),
            IExpression::EffectHandling { .. } =>
                Ok(expr),
            IExpression::Sequencing(expr, succ) =>
                Self::interpret_sequencing(defs, (expr, succ), stacks),
            IExpression::Let { id, expression } =>
                Self::interpret_let(defs, (id, expression), stacks),
            IExpression::If { guard, then_b, else_b } =>
                Self::interpret_if(defs, (guard, then_b, else_b), stacks),
            IExpression::While { guard, block } =>
                Self::interpret_while(defs, (guard, block), stacks),
            IExpression::ValueCall { expression } =>
                Self::interpret_value_call(defs, expression, stacks),
            IExpression::FuntionCall { function, arguments } =>
                Self::interpret_fn_call(defs, (function, arguments), stacks),
            IExpression::EffectCall { effect, arguments } =>
                Self::interpret_effect_call(defs, (effect, arguments), stacks),
            IExpression::HandlingInstall { handler, computation } =>
                Self::interpret_handler_install(defs, (handler, computation), stacks),
            IExpression::UnaryOp(op, expr) =>
                Self::interpret_unary_op(defs, (op, expr), stacks),
            IExpression::BinaryOp(lexpr, op, rexpr) =>
                Self::interpret_binary_op(defs, (lexpr, op, rexpr), stacks),
            IExpression::Block(expr) =>
                Self::interpret_block(defs, expr, stacks),
            IExpression::Handling(expr) =>
                Self::interpret_handling(defs, expr, stacks),
        }
    }

    fn interpret_value(_defs: &Declarations, value: Box<IValue>, stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        if let IValue::Var(id) = *value {
            let value = Self::search_identifier(&id, stacks)
                .ok_or(format!("Cannot find value for variable {}", id))?;

            Ok(IExpression::Value(Box::new(value)))
        } else {
            Ok(IExpression::Value(value))
        }
    }

    fn interpret_sequencing(defs: &Declarations, (expr, succ): (Box<IExpression>, Box<IExpression>), stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        Ok(match *expr {
            IExpression::Value(_) => *succ,
            IExpression::EffectHandling { effect, arguments, computation, environment } =>
                IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::Sequencing(computation, succ))
                },
            _ => {
                let res = Self::interpret_next(defs, *expr, stacks)?;
                IExpression::Sequencing(Box::new(res), succ)
            },
        })
    }

    fn interpret_let(defs: &Declarations, (id, expr): (Identifier, Box<IExpression>), stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        Ok(match *expr {
            IExpression::Value(value) => {
                Self::new_identifier(&id, *value, stacks);
                IExpression::Value(Box::new(IValue::ULiteral))
            },
            IExpression::EffectHandling { effect, arguments, computation, environment } => {
                IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::Let { id, expression: computation })
                }
            },
            _ => {
                let res = Self::interpret_next(defs, *expr, stacks)?;
                IExpression::Let{ id, expression: Box::new(res) }
            },
        })
    }

    fn interpret_if(defs: &Declarations, (guard, then_b, else_b): (Box<IExpression>, Box<IExpression>, Box<IExpression>), stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        Ok(match *guard {
            IExpression::Value(value) => {
                match *value {
                    IValue::BLiteral(true) => {
                        Self::push_block(stacks);
                        IExpression::Block(then_b)
                    },
                    IValue::BLiteral(false) => {
                        Self::push_block(stacks);
                        IExpression::Block(else_b)
                    },
                    value => Err(format!("If expression guard, of type {:?}, is not of boolean type", Self::value_type(&value, stacks)))?
                }
            },
            IExpression::EffectHandling { effect, arguments, computation, environment } => {
                IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::If { guard: computation, then_b, else_b })
                }
            },
            _ => {
                let res = Self::interpret_next(defs, *guard, stacks)?;
                IExpression::If{ guard: Box::new(res), then_b, else_b }
            }
        })
    }

    fn interpret_while(_defs: &Declarations, (guard, block): (Box<IExpression>, Box<IExpression>), _stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        let then_b = Box::new(
            IExpression::Sequencing(
                block.clone(),
                Box::new(IExpression::While { guard: guard.clone(), block })
            )
        );

        let else_b = Box::new(
            IExpression::Value(Box::new(
                IValue::ULiteral
            ))
        );

        Ok(IExpression::If { guard, then_b, else_b })
    }

    fn interpret_fn_call(defs: &Declarations, (fn_name, arguments): (Identifier, Vec<IExpression>), stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        let (effect_data, arguments) = Self::export_fn_args_effect(arguments);
        if let Some((effect, eff_arguments, eff_environment)) = effect_data {
            Ok(IExpression::EffectHandling { effect, arguments: eff_arguments, environment: eff_environment,
                computation: Box::new(IExpression::FuntionCall { function: fn_name, arguments })
            })
        } else {
            let (arguments, reduced) = Self::interpret_fn_args(defs, arguments, stacks)?;
            if reduced {
                match defs.find_function(&fn_name) {
                    (Some(function), _) => {
                        if function.arguments.len() != arguments.len() {
                            Err(
                                format!("Argument number mismatch for function: {}. Expected {} arguments, but found {}.",
                                function.name, function.arguments.len(), arguments.len())
                            )?;
                        }
        
                        let arguments = Self::fn_args_to_values(arguments);
        
                        Self::push_block(stacks);
                        for (id, value) in function.arguments.iter().zip(arguments.into_iter()) {
                            Self::new_identifier(id, value, stacks);
                        }
        
                        Ok(IExpression::Block(function.expression.clone().into()))
                    },
                    (None, Some(native_function)) => {
                        let arguments = Self::fn_args_to_values(arguments);

                        native_function.0(arguments)
                            .map(|value| IExpression::Value(Box::new(value)))
                    },
                    _ => Err(format!("Cannot find function with name: {}", fn_name))
                }
            } else {
                Ok(IExpression::FuntionCall { function: fn_name, arguments })
            }
        }
    }

    fn interpret_fn_args(defs: &Declarations, arguments: Vec<IExpression>, stacks: &mut Vec<EnvBlock>) -> Result<(Vec<IExpression>, bool), String> {
        let mut reduced = true;
        arguments.into_iter()
            .fold(Ok(Vec::new()), |vect, expr| {
                let mut vect = vect?;

                if !reduced {
                    vect.push(expr);
                    return Ok(vect);
                }

                match expr {
                    IExpression::Value(_) => {
                        vect.push(expr);
                    },
                    _ => {
                        let res = Self::interpret_next(defs, expr, stacks)?;
                        vect.push(res);
                        reduced = false;
                    }
                }

                Ok(vect)
            }).map(|vec| (vec, reduced))
    }

    fn fn_args_to_values(arguments: Vec<IExpression>) -> Vec<IValue> {
        arguments.into_iter().map(|expr| {
            let IExpression::Value(value) = expr else { panic!(); };
            *value
        }).collect()
    }

    fn export_fn_args_effect(arguments: Vec<IExpression>) -> (Option<(Effect, Vec<IValue>, Vec<EnvBlock>)>, Vec<IExpression>) {
        let mut reduced = true;
        let mut out_effect = None;
        let arguments = arguments.into_iter()
            .fold(Vec::new(), |mut vect, expr| {
                match (reduced, expr) {
                    (true, IExpression::EffectHandling { effect, arguments, computation, environment }) => {
                        vect.push(*computation);
                        out_effect = Some((effect, arguments, environment));
                        reduced = false;
                    },
                    (_, expr) => {
                        vect.push(expr);
                    },
                };

                vect
            });

        (out_effect, arguments)
    }

    fn interpret_effect_call(defs: &Declarations, (effect, arguments): (Effect, Vec<IExpression>), stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        let (effect_data, arguments) = Self::export_fn_args_effect(arguments);
        if let Some((out_effect, out_arguments, out_environment)) = effect_data {
            Ok(IExpression::EffectHandling { effect: out_effect, arguments: out_arguments, environment: out_environment,
                computation: Box::new(IExpression::EffectCall { effect, arguments })
            })
        } else {
            let (arguments, reduced) = Self::interpret_fn_args(defs, arguments, stacks)?;

            if reduced {
                let arguments = Self::fn_args_to_values(arguments);

                let computation = Box::new(IExpression::Value(
                    Box::new(IValue::Var("$effret".into()))
                ));

                Ok(IExpression::EffectHandling { effect, arguments, computation, environment: Vec::new() })
            } else {
                Ok(IExpression::EffectCall { effect, arguments })
            }
        }
    }

    fn interpret_handler_install(_defs: &Declarations, (handler, computation): (Identifier, Box<IExpression>), stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        Self::push_handler(handler, stacks);
        Ok(IExpression::Handling(computation))
    }

    fn interpret_unary_op(defs: &Declarations, (op, expr): (UnaryOp, Box<IExpression>), stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        match *expr {
            IExpression::Value(value) =>
                Self::execute_unary_op(op, *value, stacks),
            IExpression::EffectHandling { effect, arguments, computation, environment } => {
                Ok(IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::UnaryOp(op, computation))
                })
            },
            _ => {
                let res = Self::interpret_next(defs, *expr, stacks)?;
                Ok(IExpression::UnaryOp(op, Box::new(res)))
            }
        }
    }

    fn execute_unary_op(op: UnaryOp, value: IValue, stacks: &Vec<EnvBlock>) -> Result<IExpression, String> {
        match (op, value) {
            (UnaryOp::LNot, IValue::BLiteral(value)) => Ok(IExpression::Value( Box::new(IValue::BLiteral(!value)) )),
            (op, value) => Err(format!("Unmatched unary operator {:?} to expression of type {:?}", op, Self::value_type(&value, stacks)))
        }
    }

    fn interpret_binary_op(defs: &Declarations, (lexpr, op, rexpr): (Box<IExpression>, BinaryOp, Box<IExpression>), stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        match (*lexpr, *rexpr) {
            (IExpression::Value(lvalue), IExpression::Value(rvalue)) => {
                Self::execute_binary_op(*lvalue, op, *rvalue, stacks)
            },
            (lexpr @ IExpression::Value(_), IExpression::EffectHandling { effect, arguments, computation, environment }) => {
                Ok(IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::BinaryOp(Box::new(lexpr), op, computation))
                })
            },
            (lexpr @ IExpression::Value(_), rexpr) => {
                let rres = Self::interpret_next(defs, rexpr, stacks)?;
                Ok(IExpression::BinaryOp(Box::new(lexpr), op, Box::new(rres)))
            },
            (IExpression::EffectHandling { effect, arguments, computation, environment }, rexpr) => {
                Ok(IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::BinaryOp(computation, op, Box::new(rexpr)))
                })
            },
            (lexpr, rexpr) => {
                let lres = Self::interpret_next(defs, lexpr, stacks)?;
                Ok(IExpression::BinaryOp(Box::new(lres), op, Box::new(rexpr)))
            },
        }
    }

    fn execute_binary_op(lexpr: IValue, op: BinaryOp, rexpr: IValue, stacks: &Vec<EnvBlock>) -> Result<IExpression, String> {
        (match (lexpr, op, rexpr) {
            (IValue::BLiteral(lvalue), BinaryOp::LAnd, IValue::BLiteral(rvalue)) => Ok(IValue::BLiteral(lvalue && rvalue)),
            (lexpr, op, rexpr) => Err(format!("Unmatched binary operator {:?} to operands of type {:?} and {:?}", op, Self::value_type(&lexpr, stacks), Self::value_type(&rexpr, stacks)))
        }).map(|value| {
            IExpression::Value( Box::new(value) )
        })
    }

    fn interpret_block(defs: &Declarations, expr: Box<IExpression>, stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        match *expr {
            IExpression::Value(_) => {
                Self::pop_block(stacks);
                Ok(*expr)
            },
            IExpression::EffectHandling { effect, arguments, computation, environment } => {
                Ok(IExpression::EffectHandling { effect, arguments, environment,
                    computation: Box::new(IExpression::Block(computation))
                })
            },
            _ => {
                let res = Self::interpret_next(defs, *expr, stacks)?;
                Ok(IExpression::Block(Box::new(res)))
            }
        }
    }

    fn interpret_handling(defs: &Declarations, expr: Box<IExpression>, stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        match *expr {
            IExpression::Value(value) => {
                let handler = Self::get_handler(defs, stacks)
                    .ok_or(format!("Definition for handler {} not found!", Self::get_handler_name(stacks)))?;

                if let Some((_, _, id, expr)) = &handler.return_handler {
                    let iexpr: IExpression = (*expr.clone()).into();
                    Self::pop_handler(stacks);
                    Self::push_block(stacks);
                    Self::new_identifier(id, *value, stacks);
                    Ok(iexpr)
                } else {
                    Ok(IExpression::Value(value))
                }
            },
            IExpression::EffectHandling { effect, arguments, computation, mut environment } => {
                let handler = Self::get_handler(defs, stacks)
                    .ok_or(format!("Definition for handler {} not found!", Self::get_handler_name(stacks)))?;

                let handler_effect = handler.effect_handlers.iter()
                    .find(|&(handler_effect, _, _)| {
                        handler_effect == &effect
                    }).cloned();

                if let Some((_, handler_args, handler_body)) = handler_effect {
                    let call_stack = Self::detach_blocks(stacks);
                    for (argument_name, argument) in handler_args.iter().zip(arguments.into_iter()) {
                        Self::new_identifier(argument_name, argument, stacks);
                    }

                    let continuation = IValue::Continuation {
                        expression: computation,
                        previous_environment: environment,
                        call_stack
                    };
                    Self::new_identifier(&"continuation".into(), continuation, stacks);

                    Ok(IExpression::Block(handler_body.into()))
                } else {
                    environment.push( Self::pop_handler(stacks) );
                    Ok(IExpression::EffectHandling { effect, arguments, environment,
                        computation: Box::new(IExpression::Handling(computation))
                    })
                }
            },
            _ => {
                let res = Self::interpret_next(defs, *expr, stacks)?;
                Ok(IExpression::Handling(Box::new(res)))
            }
        }
    }

    fn interpret_value_call(defs: &Declarations, expr: Box<IExpression>, stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        match *expr {
            IExpression::Value(value) => {
                match *value {
                    IValue::Continuation { expression, previous_environment, call_stack } => {
                        Self::attach_blocks(stacks, call_stack);
                        Self::restore_environment(stacks, previous_environment);
                        Self::push_block(stacks);

                        Ok(IExpression::Block(expression))
                    },
                    val => {
                        Err(format!("Value calls can only be performed on continuations, got: {:?}", Self::value_type(&val, stacks)))
                    }
                }
            },
            _ => {
                let res = Self::interpret_next(defs, *expr, stacks)?;
                Ok(IExpression::ValueCall { expression: Box::new(res) })
            }
        }
    }

    fn value_type(value: &IValue, stacks: &Vec<EnvBlock>) -> Type {
        match value {
            IValue::ULiteral => Type::Unit,
            IValue::BLiteral(_) => Type::Bool,
            IValue::I32Literal(_) => Type::I32,
            IValue::Var(id) => {
                let Some(value) = Self::search_identifier(id, stacks) else { panic!() };
                Self::value_type(&value, stacks)
            },
            IValue::RuneLiteral(_) => Type::Rune,
            IValue::StringLiteral(_) => Type::String,
            IValue::Continuation{ expression: expr, .. } => Type::Computation(Box::new(
                Self::expression_type(expr.as_ref(), stacks)
            )),
        }
    }

    fn new_identifier(id: &Identifier, value: IValue, stacks: &mut Vec<EnvBlock>) {
        stacks.last_mut().unwrap().new_identifier(id, value);
    }

    fn search_identifier(id: &Identifier, stacks: &Vec<EnvBlock>) -> Option<IValue> {
        stacks.iter().rev().fold(None, |acc, envblock| {
            acc.or_else(|| envblock.search_identifier(id))
        })
    }

    fn push_block(stacks: &mut Vec<EnvBlock>) {
        stacks.last_mut().unwrap().push();
    }

    fn pop_block(stacks: &mut Vec<EnvBlock>) {
        stacks.last_mut().unwrap().pop();
    }

    fn push_handler(handler: Identifier, stacks: &mut Vec<EnvBlock>) {
        stacks.push(EnvBlock::new(handler))
    }

    fn pop_handler(stacks: &mut Vec<EnvBlock>) -> EnvBlock {
        stacks.pop().unwrap()
    }

    fn detach_blocks(stacks: &mut Vec<EnvBlock>) -> Vec<ActivationRecord> {
        stacks.last_mut().unwrap().detach_blocks()
    }

    fn attach_blocks(stacks: &mut Vec<EnvBlock>, call_stack: Vec<ActivationRecord>) {
        stacks.last_mut().unwrap().attach_blocks(call_stack);
    }

    fn restore_environment(stacks: &mut Vec<EnvBlock>, previous_environment: Vec<EnvBlock>) {
        stacks.extend(previous_environment.into_iter().rev())
    }

    fn get_handler<'a>(defs: &'a Declarations, stacks: &mut Vec<EnvBlock>) -> Option<&'a HandlerDeclaration> {
        stacks.last().unwrap().get_handler(defs)
    }

    fn get_handler_name(stacks: &mut Vec<EnvBlock>) -> &Identifier {
        stacks.last().unwrap().get_handler_name()
    }

    fn expression_type(_expr: &IExpression, _stacks: &Vec<EnvBlock>) -> ComputationType {
        ComputationType { typ: Type::Void, effects: ComputationEffects::AtMost(HashSet::new()) }
    }
}

impl Declarations {
    pub fn new(defs: Vec<Declaration>, native_fns: Vec<(Identifier, NativeFun)>) -> Self {
        let mut declarations = Self {
            functions: HashMap::new(),
            handlers: HashMap::new(),
            effects: HashMap::new(),
            native_functions: HashMap::new(),
        };

        defs.into_iter().for_each(|definition| {
            match definition {
                Declaration::Function(def) => { declarations.functions.insert(def.name.clone(), def); },
                Declaration::Handler(def) => { declarations.handlers.insert(def.name.clone(), def); },
                Declaration::Effect(def) => { declarations.effects.insert(def.name.clone(), def); },
            }
        });

        for (id, native_fn) in native_fns.into_iter() {
            declarations.native_functions.insert(id, native_fn);
        }

        declarations
    }

    pub fn run(&self) -> Result<String, String> {
        let Some(main) = self.find_main_function() else { return Err(format!("Main function not found!")); };
        let env = self.prepare_environment(*main.expression);
        env.finish()
            .map(|value| format!("Program returned {}", self.value_to_string(value)))
    }

    pub fn find_function(&self, function_name: &Identifier) -> (Option<&FunDeclaration>, Option<&NativeFun>) {
        (self.functions.get(function_name), self.native_functions.get(function_name))
    }

    pub fn standard_library() -> Vec<(Identifier, NativeFun)> {
        vec![
            ("nativePrint".into(), NativeFun(&|values| {
                if values.len() != 1 {
                    Err(format!("nativePrint funcion accepts only one value!"))
                } else {
                    match values.into_iter().next().unwrap() {
                        IValue::ULiteral => println!("()"),
                        IValue::BLiteral(value) => println!("{}", value),
                        IValue::I32Literal(value) => println!("{}", value),
                        IValue::Var(id) => println!("variable {}", id),
                        IValue::RuneLiteral(value) => println!("{}", value),
                        IValue::StringLiteral(value) => println!("{}", value),
                        IValue::Continuation { .. } => println!("computation"),
                    };

                    Ok(IValue::ULiteral)
                }
            }))
        ]
    }

    fn find_main_function(&self) -> Option<FunDeclaration> {
        self.functions.get(&"main".into()).cloned()
            .filter(|main_fn| main_fn.arguments.is_empty())
    }

    fn prepare_environment(&self, expr: Expression) -> Environment {
        Environment::new(self.clone(), expr)
    }

    fn value_to_string(&self, value: IValue) -> String {
        match value {
            IValue::ULiteral => format!("(): unit"),
            IValue::BLiteral(value) => format!("{}: bool", value),
            IValue::I32Literal(value) => format!("{}: i32", value),
            IValue::Var(id) => format!("{}: var", id),
            IValue::RuneLiteral(_) => todo!(),
            IValue::StringLiteral(_) => todo!(),
            IValue::Continuation { .. } => format!("!: continuation"),
        }
    }
}