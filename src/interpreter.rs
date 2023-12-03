use std::collections::HashMap;

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
        if let IExpression::Value(_) = self.expression { true } else { false }
    }

    pub fn finish(mut self) -> Result<IValue, String> {
        while self.has_next() {
            self = self.next()?;
        }
        
        let IExpression::Value(val) = self.expression else { panic!() };
        Ok(*val)
    }

    fn interpret_next(defs: &Declarations, expr: IExpression, stacks: &mut Vec<EnvBlock>) -> Result<IExpression, String> {
        Ok(match expr {
            IExpression::Value(_) => expr,
            IExpression::Sequencing(expr, succ) => {
                if let IExpression::Value(_) = *expr {
                    *succ
                } else {
                    let res = Self::interpret_next(defs, *expr, stacks)?;
                    IExpression::Sequencing(Box::new(res), succ)
                }
            },
            IExpression::Let { id, expression } => todo!(),
            IExpression::If { guard, then_b, else_b } => todo!(),
            IExpression::While { guard, block } => todo!(),
            IExpression::FunCall { function, arguments } => todo!(),
            IExpression::EffCall { effect, arguments } => todo!(),
            IExpression::Handling { handler, computation } => todo!(),
            IExpression::UnaryOp(op, expr) => {
                if let IExpression::Value(value) = *expr {
                    Self::interpret_unary_op(op, *value, stacks)?
                } else {
                    let res = Self::interpret_next(defs, *expr, stacks)?;
                    IExpression::UnaryOp(op, Box::new(res))
                }
            },
            IExpression::BinaryOp(lexpr, op, rexpr) => {
                match (*lexpr, *rexpr) {
                    (IExpression::Value(lvalue), IExpression::Value(rvalue)) => {
                        Self::interpret_binary_op(*lvalue, op, *rvalue, stacks)?
                    },
                    (lexpr @ IExpression::Value(_), rexpr) => {
                        let rres = Self::interpret_next(defs, rexpr, stacks)?;
                        IExpression::BinaryOp(Box::new(lexpr), op, Box::new(rres))
                    },
                    (lexpr, rexpr) => {
                        let lres = Self::interpret_next(defs, lexpr, stacks)?;
                        IExpression::BinaryOp(Box::new(lres), op, Box::new(rexpr))
                    },
                }
            },
            IExpression::Body(expr) => {
                if let IExpression::Value(_) = *expr {
                    *expr
                } else {
                    let res = Self::interpret_next(defs, *expr, stacks)?;
                    IExpression::Body(Box::new(res))
                }
            },
        })
    }

    fn interpret_unary_op(op: UnaryOp, value: IValue, stacks: &Vec<EnvBlock>) -> Result<IExpression, String> {
        match (op, value) {
            (UnaryOp::LNot, IValue::BLiteral(value)) => Ok(IExpression::Value( Box::new(IValue::BLiteral(!value)) )),
            (op, value) => Err(format!("Unmatched unary operator {:?} to expression of type {:?}", op, Self::value_type(&value, stacks)))
        }
    }

    fn interpret_binary_op(lexpr: IValue, op: BinaryOp, rexpr: IValue, stacks: &Vec<EnvBlock>) -> Result<IExpression, String> {
        (match (lexpr, op, rexpr) {
            (IValue::BLiteral(lvalue), BinaryOp::LAnd, IValue::BLiteral(rvalue)) => Ok(IValue::BLiteral(lvalue && rvalue)),
            (lexpr, op, rexpr) => Err(format!("Unmatched binary operator {:?} to operands of type {:?} and {:?}", op, Self::value_type(&lexpr, stacks), Self::value_type(&rexpr, stacks)))
        }).map(|value| {
            IExpression::Value( Box::new(value) )
        })
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
            IValue::Continuation(expr, _) => Type::Computation(Box::new(
                Self::expression_type(expr.as_ref(), stacks)
            )),
        }
    }

    fn search_identifier(id: &Identifier, stacks: &Vec<EnvBlock>) -> Option<IValue> {
        stacks.iter().rev().fold(None, |acc, envblock| {
            acc.or_else(|| envblock.search_identifier(id))
        })
    }

    fn expression_type(expr: &IExpression, stacks: &Vec<EnvBlock>) -> ComputationType {
        todo!()
    }
}

impl Declarations {
    pub fn new(defs: Vec<Declaration>) -> Self {
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

        declarations
    }

    pub fn run(&self) {
        let Some(main) = self.find_main_function() else { print!("Main function not found!"); return; };
        let env = self.prepare_environment(*main.expression);
        match env.finish() {
            Ok(value) => {
                print!("Program returned {}", self.value_to_string(value));
            },
            Err(err) => {
                print!("{err}");
            },
        }
    }

    fn find_main_function(&self) -> Option<FunDeclaration> {
        self.functions.get(&"main".into()).cloned()
            .filter(|main_fn| main_fn.arguments.is_empty())
    }

    fn interpret(&self, env: &mut Environment, expr: Expression) -> Result<Value, String> {
        match expr {
            Expression::Value(value) => Ok(*value),
            Expression::Sequencing(left, right) => {
                let left_res = self.interpret(env, *left)?;
                if left_res != Value::ULiteral {
                    return Err("Exception: Sequencing left expression returned a non unit value.".to_owned());
                }

                self.interpret(env, *right)
            },
            Expression::Let { id, expression } => todo!(),
            Expression::If { guard, then_b, else_b } => {
                match self.interpret(env, *guard)? {
                    Value::BLiteral(true) => self.interpret(env, *then_b),
                    Value::BLiteral(false) => self.interpret(env, *else_b),
                    _ => Err("Exception: If guard expression returned a non bool value.".to_owned())
                }
            },
            Expression::While { guard, block } => {
                loop {
                    match self.interpret(env, *guard.clone())? {
                        Value::BLiteral(true) => { 
                            let block_res = self.interpret(env, *block.clone())?;
                            if block_res != Value::ULiteral {
                                return Err("Exception: While block expression returned a non unit value.".to_owned());
                            }
                        },
                        Value::BLiteral(false) => { return Ok(Value::ULiteral); },
                        _ => { return Err("Exception: While guard expression returned a non bool value.".to_owned()); }
                    }
                }
            },
            Expression::FunCall { function, arguments } => {
                let fun_def = self.functions.get(&function)
                    .ok_or(format!("function {} not found", function.0));

                if fun_def.is_ok() {
                    let arguments = self.extract_arguments(env, arguments)?;

                    todo!()
                } else {
                    let &NativeFun(native_fun) = self.native_functions.get(&function)
                        .ok_or(format!("native function {} not found", function.0))?;

                    let arguments = self.extract_arguments(env, arguments)?;
                    native_fun(arguments)
                }              
            },
            Expression::EffCall { effect, arguments } => {
                let effect_def = self.effects.get(&effect.eff_type)
                    .ok_or( format!("Effect {} not found", effect.eff_type.0) )
                    .and_then(|eff| {
                        if eff.in_types.len() == arguments.len() {
                            Ok(eff)
                        } else {
                            Err( format!("Effect call, for effect {}, argument number mismatch", effect.eff_type.0) )
                        }
                    })?;

                let arguments = self.extract_arguments(env, arguments)?;
                if effect_def.in_types.iter().zip(arguments.iter())
                    .all(|(in_type, arg)| &self.value_type(env, arg) == in_type ) {
                    return Err(format!("Effect call, for effect {}, argument type mismatch.", effect.eff_type.0));
                }

                todo!()
            },
            Expression::Handling { handler, computation } => {
                todo!()
            },
            Expression::UnaryOp(op, expr) => {
                let value = self.interpret(env, *expr)?;
                let value = self.value_extract(env, value)?;

                match (op, value) {
                    (UnaryOp::LNot, Value::BLiteral(value)) => Ok(Value::BLiteral(!value)),
                    _ => Err(format!("Unrecognised unary operator expression"))
                }
            },
            Expression::BinaryOp(lexpr, op, rexpr) => {
                let (lvalue, rvalue) = (self.interpret(env, *lexpr)?, self.interpret(env, *rexpr)?);
                let (lvalue, rvalue) = (self.value_extract(env, lvalue)?, self.value_extract(env, rvalue)?);
                
                match (lvalue, op, rvalue) {
                    (Value::I32Literal(lvalue), BinaryOp::Add, Value::I32Literal(rvalue)) =>
                        Ok(Value::I32Literal(lvalue + rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Sub, Value::I32Literal(rvalue)) =>
                        Ok(Value::I32Literal(lvalue - rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Mul, Value::I32Literal(rvalue)) =>
                        Ok(Value::I32Literal(lvalue * rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Div, Value::I32Literal(rvalue)) =>
                        Ok(Value::I32Literal(lvalue / rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Mod, Value::I32Literal(rvalue)) =>
                        Ok(Value::I32Literal(lvalue % rvalue)),

                    (Value::BLiteral(lvalue), BinaryOp::LAnd, Value::BLiteral(rvalue)) =>
                        Ok(Value::BLiteral(lvalue && rvalue)),
                    (Value::BLiteral(lvalue), BinaryOp::LOr, Value::BLiteral(rvalue)) =>
                        Ok(Value::BLiteral(lvalue || rvalue)),
                    (Value::BLiteral(lvalue), BinaryOp::LXor, Value::BLiteral(rvalue)) =>
                        Ok(Value::BLiteral(lvalue != rvalue)),

                    (Value::BLiteral(lvalue), BinaryOp::Eq, Value::BLiteral(rvalue)) =>
                        Ok(Value::BLiteral(lvalue == rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Eq, Value::I32Literal(rvalue)) =>
                        Ok(Value::BLiteral(lvalue == rvalue)),
                    (Value::BLiteral(lvalue), BinaryOp::Ne, Value::BLiteral(rvalue)) =>
                        Ok(Value::BLiteral(lvalue != rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Ne, Value::I32Literal(rvalue)) =>
                        Ok(Value::BLiteral(lvalue != rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Gt, Value::I32Literal(rvalue)) =>
                        Ok(Value::BLiteral(lvalue > rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Ge, Value::I32Literal(rvalue)) =>
                        Ok(Value::BLiteral(lvalue >= rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Lt, Value::I32Literal(rvalue)) =>
                        Ok(Value::BLiteral(lvalue < rvalue)),
                    (Value::I32Literal(lvalue), BinaryOp::Le, Value::I32Literal(rvalue)) =>
                        Ok(Value::BLiteral(lvalue <= rvalue)),
                    _ => Err(format!("Unrecognised binary operator expression"))
                }
            },
        }
    }

    fn prepare_environment(&self, expr: Expression) -> Environment {
        Environment::new(self.clone(), expr)
    }
    
    fn value_type(&self, env: &mut Environment, value: &Value) -> Type {
        match value {
            Value::ULiteral => Type::Unit,
            Value::BLiteral(_) => Type::Bool,
            Value::I32Literal(_) => Type::I32,
            Value::Var(_) => todo!(),
            Value::RuneLiteral(_) => Type::Rune,
            Value::StringLiteral(_) => Type::String,
        }
    }

    fn value_to_string(&self, value: IValue) -> String {
        todo!()
    }

    fn value_extract(&self, env: &mut Environment, value: Value) -> Result<Value, String> {
        match value {
            Value::Var(value) => todo!(),
            _ => Ok(value)
        }
    }

    fn extract_arguments(&self, env: &mut Environment, args: Vec<Expression>) -> Result<Vec<Value>, String> {
        args.into_iter()
            .fold(Ok(Vec::new()), |vec, arg| {
                if vec.is_err() { vec }
                else {
                    match self.interpret(env, arg) {
                        Ok(arg) => {
                            let mut vec = vec.unwrap();
                            vec.push(arg);
                            Ok(vec)
                        },
                        Err(err) => Err(err),
                    }
                } 
            })
    }
}