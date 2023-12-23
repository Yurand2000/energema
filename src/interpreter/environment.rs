use super::*;

impl Environment {
    pub fn new(declarations: Declarations) -> Self {
        let mut env = Self { declarations, call_stack: Vec::new() };
        env.reset();
        env
    }

    pub fn reset(&mut self) {
        self.call_stack.clear();
        self.call_stack.push(EnvBlock::new(Identifier("default_handler".to_owned())))
    }

    pub fn new_identifier(&mut self, id: &Identifier, value: IValue) {
        self.call_stack.last_mut().unwrap().new_identifier(id, value);
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

    pub fn detach_blocks(&mut self) -> Vec<ActivationRecord> {
        self.call_stack.last_mut().unwrap().detach_blocks()
    }

    pub fn attach_blocks(&mut self, call_stack: Vec<ActivationRecord>) {
        self.call_stack.last_mut().unwrap().attach_blocks(call_stack);
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