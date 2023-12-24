use std::fmt::Debug;

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

    pub fn pop_identifier(&mut self, id: &Identifier) -> Option<IValue> {
        self.0.remove(&id)
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

    pub fn pop_identifier(&mut self, id: &Identifier) -> Option<IValue> {
        self.stack.iter_mut().rev().fold(None, |acc, act_record| {
            acc.or_else(|| act_record.pop_identifier(id))
        })
    }

    pub(super) fn get_handler<'a>(&self, env: &'a Environment) -> Option<&'a IHandlerDeclaration> {
        env.search_handler(&self.handler)
    }

    pub fn get_handler_name(&self) -> &Identifier {
        &self.handler
    }

    pub fn attach(&mut self, record: ActivationRecord) {
        self.stack.push(record);
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

    pub fn get_stack_size(&self) -> usize {
        self.stack.len()
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
    Closure{ arguments: Vec<Identifier>, computation: Box<IExpression>, environment: ActivationRecord },
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
    FunctionCall{ function: Box<IExpression>, arguments: Vec<IExpression> },
    EffectCall{ effect: Effect, arguments: Vec<IExpression> },                  //substitute with VarValue($effret), transform the expression(from the root) to a continuation value
    HandlingInstall{ handler: Identifier, computation: Box<IExpression> },      //substitute with Handling(Block(expression))
    ClosureCreate{ arguments: Vec<Identifier>, computation: Box<IExpression> }, //substitute with Value(Closure)
    BlockCreate( Box<IExpression> ),                                            //substitute with Block

    UnaryOp(UnaryOp, Box<IExpression>),
    BinaryOp(Box<IExpression>, BinaryOp, Box<IExpression>),

    //Non-Constructible by the parser
    Block( Box<IExpression> ),
    Handling( Box<IExpression> ),
    EffectHandling{ effect: Effect, arguments: Vec<IValue>, computation: Box<IExpression>, environment: Vec<EnvBlock> },
    Continuation{ expression: Box<IExpression>, previous_environment: Vec<EnvBlock> },
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
            Expression::Block(expression) => IExpression::BlockCreate(expression.into()),
            Expression::Closure { arguments, computation } => IExpression::ClosureCreate { arguments, computation: computation.into() },
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

struct Pad<T>(T, bool);

impl<T: std::fmt::Display> std::fmt::Display for Pad<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = format!("{}", self.0);
        let mut lines = text.lines();
        let mut text = String::new();
        if self.1 { text.push_str("- "); }
        else { text.push_str("  "); }
        text.push_str(lines.next().unwrap());
        text.push('\n');
        let mut text = lines.fold(text,
            |mut accum, line| {
                accum.push_str("  ");
                accum.push_str(line);
                accum.push('\n');
                accum
        });
        text.pop();
        f.write_str(&text)
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
                f.write_fmt(format_args!("Sequencing(\n{}, \n{}\n)", Pad(pre, true), Pad(post, true))),
            IExpression::Let { id, expression } =>
                f.write_fmt(format_args!("Let {} =\n{}", id.0, Pad(expression, false))),
            IExpression::If { guard, then_b, else_b } =>
                f.write_fmt(format_args!("If {}\n{}\nelse\n{}", guard, Pad(then_b, true), Pad(else_b, true))),
            IExpression::While { guard, block } =>
                f.write_fmt(format_args!("While {}\n{}", guard, Pad(block, false))),
            IExpression::FunctionCall { function, arguments } =>
                f.write_fmt(format_args!("Call {}\n{}", function, Pad(Arguments(arguments), false))),
            IExpression::EffectCall { effect, arguments } =>
                f.write_fmt(format_args!("EffCall {:?}\n{}", effect, Pad(Arguments(arguments), false))),
            IExpression::HandlingInstall { handler, computation } =>
                f.write_fmt(format_args!("With {}() handle [leaf in tree]\n{}", handler.0, Pad(computation, false))),
            IExpression::UnaryOp(op, expr) =>
                f.write_fmt(format_args!("{:?}{}", op, expr)),
            IExpression::BinaryOp(lexpr, op, rexpr) =>
                f.write_fmt(format_args!("{:?}(\n{}, \n{}\n)", op, Pad(lexpr, true), Pad(rexpr, true))),
            IExpression::Block(expr) =>
                f.write_fmt(format_args!("Block {{\n{}\n}}", Pad(expr, false))),
            IExpression::BlockCreate(expr) =>
                f.write_fmt(format_args!("BlockCreate [leaf in tree]{{\n{}\n}}", Pad(expr, false))),
            IExpression::Handling(expr) =>
                f.write_fmt(format_args!("Handling \n{}", Pad(expr, false))),
            IExpression::EffectHandling { effect, arguments, computation, .. } =>
                f.write_fmt(format_args!("EffectHandling {:?}{:?} \n{}", effect, arguments, Pad(computation, false))),
            IExpression::ClosureCreate { arguments, computation } =>
                f.write_fmt(format_args!("ClosureCreate |{:?}| [leaf in tree]\n{}", arguments, Pad(computation, false))),
            IExpression::Continuation { expression, .. } =>
                f.write_fmt(format_args!("Continuation [leaf in tree]\n{}", Pad(expression, false))),
        }
    }
}

struct Arguments<'a>(&'a Vec<IExpression>);

impl std::fmt::Display for Arguments<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            write!(f, "[]")?;
        } else {
            writeln!(f, "[")?;
            for argument in self.0 {
                writeln!(f, "  {},", argument)?;
            }
            write!(f, "]")?;
        }
    
        Ok(())
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
            IValue::Closure { .. } => f.write_str("closure"),
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