use super::*;

#[derive(Clone)]
pub struct Declarations {
    functions: HashMap<Identifier, FunDeclaration>,
    handlers: HashMap<Identifier, HandlerDeclaration>,
    effects: HashMap<Identifier, EffectDeclaration>,

    native_functions: HashMap<Identifier, NativeFun>
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

    pub fn find_handler(&self, handler_name: &Identifier) -> Option<&HandlerDeclaration> {
        self.handlers.get(handler_name)
    }

    pub fn find_function(&self, function_name: &Identifier) -> (Option<&FunDeclaration>, Option<&NativeFun>) {
        (self.functions.get(function_name), self.native_functions.get(function_name))
    }

    pub fn find_main_function(&self) -> Option<FunDeclaration> {
        self.functions.get(&"main".into()).cloned()
            .filter(|main_fn| main_fn.arguments.is_empty())
    }

    pub fn value_to_string(&self, value: IValue) -> String {
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