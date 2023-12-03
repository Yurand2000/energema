use super::*;

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct ActivationRecord(HashMap<Identifier, IValue>);

impl ActivationRecord {
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
    pub fn search_identifier(&self, id: &Identifier) -> Option<IValue> {
        self.stack.iter().rev().fold(None, |acc, act_record| {
            acc.or_else(|| act_record.search_identifier(id))
        })
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum IValue {
    ULiteral,
    BLiteral(bool),
    I32Literal(i32),
    Var(Identifier),
    RuneLiteral(char),
    StringLiteral(String),

    //Non-Constructible by the parser
    Continuation(Box<IExpression>, Vec<EnvBlock>),
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub enum IExpression {
    Value( Box<IValue> ),
    Sequencing( Box<IExpression>, Box<IExpression> ),
    Let{ id: Identifier, expression: Box<IExpression> },
    If{ guard: Box<IExpression>, then_b: Box<IExpression>, else_b: Box<IExpression> },
    While{ guard: Box<IExpression>, block: Box<IExpression> },
    FunCall{ function: Identifier, arguments: Vec<IExpression> },    //substitute with Body constructor
    EffCall{ effect: Effect, arguments: Vec<IExpression> },          //substitute with Value(Var($effret)), transform the expression(from the root) to a continuation value
    Handling{ handler: Identifier, computation: Box<IExpression> },  //substitute with Body constructor

    UnaryOp(UnaryOp, Box<IExpression>),
    BinaryOp(Box<IExpression>, BinaryOp, Box<IExpression>),

    //Non-Constructible by the parser
    Body( Box<IExpression> ),
}

impl From<Value> for IValue {
    fn from(value: Value) -> Self {
        match value {
            Value::ULiteral => IValue::ULiteral,
            Value::BLiteral(val) => IValue::BLiteral(val),
            Value::I32Literal(val) => IValue::I32Literal(val),
            Value::Var(val) => IValue::Var(val),
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
            Expression::Sequencing(curr, next) => IExpression::Sequencing(curr.into(), next.into()),
            Expression::Let { id, expression } => IExpression::Let { id: id, expression: expression.into() },
            Expression::If { guard, then_b, else_b } => IExpression::If { guard: guard.into(), then_b: then_b.into(), else_b: else_b.into() },
            Expression::While { guard, block } => IExpression::While { guard: guard.into(), block: block.into() },
            Expression::FunCall { function, arguments } => IExpression::FunCall { function, arguments: IExpression::vector(arguments) },
            Expression::EffCall { effect, arguments } => IExpression::EffCall { effect, arguments: IExpression::vector(arguments) },
            Expression::Handling { handler, computation } => IExpression::Handling { handler, computation: computation.into() },
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