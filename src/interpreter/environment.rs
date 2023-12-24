use super::*;

impl Environment {
    pub fn new(declarations: Declarations) -> Self {
        let mut env = Self { declarations, call_stack: Vec::new() };
        env.reset();
        env
    }

    pub fn reset(&mut self) {
        self.call_stack.clear();
        self.call_stack.push(EnvBlock::new("$default_handler".into()))
    }

    pub fn new_identifier(&mut self, id: &Identifier, value: IValue) {
        self.call_stack.last_mut().unwrap().new_identifier(id, value);
    }
    
    pub fn new_identifier_str(&mut self, id: &str, value: IValue) {
        self.call_stack.last_mut().unwrap().new_identifier(&id.into(), value);
    }

    pub fn search_identifier(&self, id: &Identifier) -> Option<IValue> {
        self.call_stack.iter().rev()
            .fold(None, |acc, envblock| {
                acc.or_else(|| envblock.search_identifier(id))
            })
            .or_else(|| self.search_function(id))
    }

    pub fn get_main_function(&self) -> Option<IFunDeclaration> {
        self.declarations.functions.get(&"main".into()).cloned()
            .filter(|main_fn| main_fn.arguments.is_empty())
    }

    fn search_function(&self, id: &Identifier) -> Option<IValue> {
        self.declarations.functions.get(id)
            .map(|fun| IValue::Function(fun.clone()))
            .or_else(|| {
                self.declarations.native_functions.get(id)
                    .map(|fun| IValue::NativeFunction(fun.clone()))
            })
    }

    pub fn search_handler(&self, id: &Identifier) -> Option<&IHandlerDeclaration> {
        self.declarations.handlers.get(id)
    }

    pub fn search_effect(&self, effect: &Effect) -> Option<&EffectDeclaration> {
        self.declarations.effects.get(&effect.eff_type)
    }

    pub fn attach_block(&mut self, record: ActivationRecord) {
        self.call_stack.last_mut().unwrap().attach(record);
    }

    pub fn push_block(&mut self) {
        self.call_stack.last_mut().unwrap().push();
    }

    pub fn pop_block(&mut self) {
        self.call_stack.last_mut().unwrap().pop();
    }

    pub fn push_handler(&mut self, handler: Identifier) {
        self.call_stack.push(EnvBlock::new(handler))
    }

    pub fn pop_handler(&mut self) -> EnvBlock {
        self.call_stack.pop().unwrap()
    }

    pub fn restore_environment(&mut self, previous_environment: Vec<EnvBlock>) {
        self.call_stack.extend(previous_environment.into_iter().rev())
    }

    pub fn get_handler(&self) -> Option<&IHandlerDeclaration> {
        self.call_stack.last().unwrap().get_handler(&self)
    }

    pub fn get_handler_name(&self) -> &Identifier {
        self.call_stack.last().unwrap().get_handler_name()
    }
}