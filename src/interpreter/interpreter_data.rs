use std::fmt::{Debug, Pointer};

use super::*;

#[derive(Debug)]
#[derive(Default, Clone)]
#[derive(PartialEq, Eq)]
pub struct ActivationRecord(HashMap<Identifier, IValue>);

impl ActivationRecord {
    pub fn new_identifier(&mut self, id: &Identifier, value: IValue) {
        self.0.insert(id.clone(), value);
    }

    pub fn search_identifier(&self, id: &Identifier) -> Option<IValue> {
        self.0.get(&id).cloned()
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct EnvBlock {
    handler: Identifier,
    stack: Vec<ActivationRecord>,
}

impl EnvBlock {
    pub fn new(handler: Identifier) -> Self {
        Self { handler, stack: vec![ActivationRecord::default()] }
    }

    pub fn new_identifier(&mut self, id: &Identifier, value: IValue) {
        self.stack.last_mut().unwrap().new_identifier(id, value);
    }

    pub fn search_identifier(&self, id: &Identifier) -> Option<IValue> {
        self.stack.iter().rev().fold(None, |acc, act_record| {
            acc.or_else(|| act_record.search_identifier(id))
        })
    }

    pub(super) fn get_handler<'a>(&self, env: &'a Environment) -> Option<&'a IHandlerDeclaration> {
        env.search_handler(&self.handler)
    }

    pub fn get_handler_name(&self) -> &Identifier {
        &self.handler
    }

    pub fn push(&mut self) {
        self.stack.push(ActivationRecord::default());
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn detach_blocks(&mut self) -> Vec<ActivationRecord> {
        std::mem::replace(&mut self.stack, vec![ActivationRecord::default()])
    }

    pub fn attach_blocks(&mut self, call_stack: Vec<ActivationRecord>) {
        self.stack.extend(call_stack.into_iter());
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct IFunDeclaration {
    pub name: Identifier,
    pub arguments: Vec<Identifier>,
    pub expression: Box<IExpression>,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct IHandlerDeclaration {
    pub name: Identifier,
    pub return_handler: Option<(Type, Type, Identifier, Box<IExpression>)>,
    pub effect_handlers: Vec<(Effect, Vec<Identifier>, Box<IExpression>)>,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum IValue {
    ULiteral,
    BLiteral(bool),
    I32Literal(i32),
    RuneLiteral(char),
    StringLiteral(String),

    //Non-Constructible by the parser
    Function(IFunDeclaration),
    NativeFunction(NativeFun),
    Continuation{ expression: Box<IExpression>, previous_environment: Vec<EnvBlock>, call_stack: Vec<ActivationRecord> },
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum IExpression {
    Value( Box<IValue> ),
    VarValue( Identifier ),
    Sequencing( Box<IExpression>, Box<IExpression> ),
    Let{ id: Identifier, expression: Box<IExpression> },
    If{ guard: Box<IExpression>, then_b: Box<IExpression>, else_b: Box<IExpression> },
    While{ guard: Box<IExpression>, block: Box<IExpression> },
    FunctionCall{ function: Box<IExpression>, arguments: Vec<IExpression> },    //substitute with Body constructor
    EffectCall{ effect: Effect, arguments: Vec<IExpression> },                  //substitute with Value(Var($effret)), transform the expression(from the root) to a continuation value
    HandlingInstall{ handler: Identifier, computation: Box<IExpression> },      //substitute with Body constructor

    UnaryOp(UnaryOp, Box<IExpression>),
    BinaryOp(Box<IExpression>, BinaryOp, Box<IExpression>),

    //Non-Constructible by the parser
    Block( Box<IExpression> ),
    Handling( Box<IExpression> ),
    EffectHandling{ effect: Effect, arguments: Vec<IValue>, computation: Box<IExpression>, environment: Vec<EnvBlock> }
}

#[derive(Clone)]
pub struct NativeFun( pub &'static dyn Fn(Vec<IValue>) -> Result<IValue, String> );

impl std::fmt::Debug for NativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NativeFun").finish()
    }
}

impl PartialEq for NativeFun {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&self.0, &other.0) 
    }
}

impl Eq for NativeFun {}

impl From<Value> for IValue {
    fn from(value: Value) -> Self {
        match value {
            Value::ULiteral => IValue::ULiteral,
            Value::BLiteral(val) => IValue::BLiteral(val),
            Value::I32Literal(val) => IValue::I32Literal(val),
            Value::RuneLiteral(val) => IValue::RuneLiteral(val),
            Value::StringLiteral(val) => IValue::StringLiteral(val),
        }
    }
}

impl From<Box<Value>> for Box<IValue> {
    fn from(value: Box<Value>) -> Self {
        Box::new( (*value).into() )
    }
}

impl From<Expression> for IExpression {
    fn from(expr: Expression) -> Self {
        match expr {
            Expression::Value(val) => IExpression::Value( val.into() ),
            Expression::VarValue(id) => IExpression::VarValue( id ),
            Expression::Sequencing(curr, next) => IExpression::Sequencing(curr.into(), next.into()),
            Expression::Let { id, expression } => IExpression::Let { id: id, expression: expression.into() },
            Expression::If { guard, then_b, else_b } => IExpression::If { guard: guard.into(), then_b: then_b.into(), else_b: else_b.into() },
            Expression::While { guard, block } => IExpression::While { guard: guard.into(), block: block.into() },
            Expression::FunCall { function, arguments } => IExpression::FunctionCall { function: function.into(), arguments: IExpression::vector(arguments) },
            Expression::EffCall { effect, arguments } => IExpression::EffectCall { effect, arguments: IExpression::vector(arguments) },
            Expression::Handling { handler, computation } => IExpression::HandlingInstall { handler, computation: computation.into() },
            Expression::UnaryOp(op, expr) => IExpression::UnaryOp(op, expr.into()),
            Expression::BinaryOp(lexpr, op, rexpr) => IExpression::BinaryOp(lexpr.into(), op, rexpr.into()),
        }
    }
}

impl From<Box<Expression>> for Box<IExpression> {
    fn from(expr: Box<Expression>) -> Self {
        Box::new( (*expr).into() )
    }
}

impl IExpression {
    fn vector(exprs: Vec<Expression>) -> Vec<IExpression> {
        exprs.into_iter().map(|expr| expr.into()).collect()
    }
}

impl From<FunDeclaration> for IFunDeclaration {
    fn from(value: FunDeclaration) -> Self {
        IFunDeclaration { name: value.name, arguments: value.arguments, expression: value.expression.into() }
    }
}

impl From<HandlerDeclaration> for IHandlerDeclaration {
    fn from(value: HandlerDeclaration) -> Self {
        IHandlerDeclaration {
            name: value.name,
            return_handler: value.return_handler.map(|(a,b,c,expr)| (a,b,c,expr.into())),
            effect_handlers: value.effect_handlers.into_iter().map(|(a,b,expr)| (a,b,expr.into())).collect()
        }
    }
}

struct Pad<T>(T);

impl<T: std::fmt::Display> std::fmt::Display for Pad<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = format!("{}", self.0);
        f.write_str(&text.lines().fold(String::new(),
            |mut accum, line| {
                accum.push_str("  ");
                accum.push_str(line);
                accum.push('\n');
                accum
        }))
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Pad<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("  {:?}", self.0))
    }
}

impl std::fmt::Display for IExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IExpression::Value(value) =>
                f.write_fmt(format_args!("Value({})", value)),
            IExpression::VarValue(var) =>
                f.write_fmt(format_args!("Var({})", var.0)),
            IExpression::Sequencing(pre, post) =>
                f.write_fmt(format_args!("Sequencing(\n{}, \n{}\n)", Pad(pre), Pad(post))),
            IExpression::Let { id, expression } =>
                f.write_fmt(format_args!("Let {} =\n{}", id.0, Pad(expression))),
            IExpression::If { guard, then_b, else_b } =>
                f.write_fmt(format_args!("If {}\n{}\nelse\n{}", guard, Pad(then_b), Pad(else_b))),
            IExpression::While { guard, block } =>
                f.write_fmt(format_args!("While {}\n{}", guard, Pad(block))),
            IExpression::FunctionCall { function, arguments } =>
                f.write_fmt(format_args!("Call {}\n{:#?}", function, Pad(arguments))),
            IExpression::EffectCall { effect, arguments } =>
                f.write_fmt(format_args!("EffCall {:?}\n{:#?}", effect, Pad(arguments))),
            IExpression::HandlingInstall { handler, computation } =>
                f.write_fmt(format_args!("With {}() handle\n{}", handler.0, Pad(computation))),
            IExpression::UnaryOp(op, expr) =>
                f.write_fmt(format_args!("{:?}{}", op, expr)),
            IExpression::BinaryOp(lexpr, op, rexpr) =>
                f.write_fmt(format_args!("{:?}(\n{}, \n{}\n)", op, Pad(lexpr), Pad(rexpr))),
            IExpression::Block(expr) =>
                f.write_fmt(format_args!("Block {{\n{}\n}}", Pad(expr))),
            IExpression::Handling(expr) =>
                f.write_fmt(format_args!("Handling {{\n{}\n}}", Pad(expr))),
            IExpression::EffectHandling { effect, arguments, computation, .. } =>
                f.write_fmt(format_args!("Effect {:?}({:?}) {{\n{}\n}}", effect, arguments, Pad(computation))),
        }
    }
}

impl std::fmt::Display for IValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IValue::ULiteral => f.write_str("unit"),
            IValue::BLiteral(value) => f.write_fmt(format_args!("{}", value)),
            IValue::I32Literal(value) => f.write_fmt(format_args!("{}", value)),
            IValue::RuneLiteral(value) => f.write_fmt(format_args!("'{}'", value)),
            IValue::StringLiteral(value) => f.write_fmt(format_args!("\"{}\"", value)),
            IValue::Function(value) => f.write_fmt(format_args!("{}", value)),
            IValue::NativeFunction(value) => f.write_fmt(format_args!("{}", value)),
            IValue::Continuation { .. } => f.write_str("continuation"),
        }
    }
}

impl std::fmt::Display for IFunDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("fn {}", self.name))
    }
}

impl std::fmt::Display for NativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("native fn")
    }
}